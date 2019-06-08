import re
import logging



class ValidatingParser(object):

    @classmethod
    def truncate(cls, source):
        if len(source) > 40:
            return source[:20] + '...' + source[-20:]
        else:
            return source

    @classmethod
    def validate_notebook(cls, nb, dict_cells, buffer_text, cell_cls):
        """
        Used to validate parsing process with a notebook ipynb file is
        loaded by comparing the notebook object to the parse result. If
        validation fails, returns an error message otherwise returns
        None.
        """
        ipynb_modes = [c['cell_type'] for c in nb.cells]
        ipynb_sources =  [c['source'] for c in nb.cells]
        if  ipynb_modes != [c['mode'] for c in dict_cells]:
            print("Very surprising! Not redundant?")
        if ipynb_sources != [c['source'] for c in dict_cells]:
            print("Even more surprising! Not redundant?")

        parsed_cells = cls.extract_cells(buffer_text, cell_cls, unique_prompts=True)
        parsed_modes = [c['mode'] for c in parsed_cells]
        parsed_sources = [c['source'] for c in parsed_cells]
        if ipynb_modes != parsed_modes:
            logging.info('!!WARNING!!\nMismatch between JSON and parsed cell types:\n'
                         + ('\tIPYNB: %s\n' % ipynb_modes)
                         + ('\tPARSED: %s' % parsed_modes))
            return False


        for ipy_source, parse_source in zip(ipynb_sources, parsed_sources):
            if ipy_source!=parse_source:
                lenmsg = (('!!WARNING!!\nMismatch between JSON source [length %d] and '
                          'parsed cell source [length %d]:\n')
                          % (len(ipy_source), len(parse_source)))
                logging.info(lenmsg
                         + ('\tIPYNB: %r\n' % cls.truncate(ipy_source)
                         + ('\tPARSED: %r' %  cls.truncate(parse_source))))
                return False
        return True


class ParseNotebook(ValidatingParser):

    @classmethod
    def find_markdown_range(cls, start, stop, lines):
        "Look backwards from stop index to start index for markdown opener"
        md_start = None
        for i in list(range(stop-1, start-1,-1)):
            if lines[i].startswith("\"\"\""): # REGEXP TO TERMINATE
                md_start = i
                break

        return None if md_start is None else (md_start, stop, 'markdown', {})

    @classmethod
    def filter_out_markdown_ranges(cls, lines, ranges):
        """
        As markdown is the first pass so all lines should be present.

        This function filters out the identified markdown ranges from all
        the lines to find out what is remaining (potential code cells).
        """
        all_line_numbers = list(range(len(lines)))
        accounted_lines = [list(range(start, stop+1)) for (start, stop, _, _) in ranges]
        flattened = [el for group in accounted_lines for el in group]
        remaining_line_numbers = set(all_line_numbers) - set(flattened)
        return [(el, lines[el]) for el in sorted(remaining_line_numbers)]

    @classmethod
    def find_markdown_ranges(cls, lines):
        """
        Return a list of range tuples (start, stop, 'md') and a list of
        remaining (numbered) lines
        """
        numbered_lines = list(enumerate(lines))
        md_ranges = []
        start = 0
        for ind, line in numbered_lines:
            if line.startswith("\"\"\" #:md:"): # REGEXP TO TERMINATE
                md_range = cls.find_markdown_range(start, ind, lines)
                if md_range is None: continue
                else:
                    (start, _, _, _ ) = md_range
                    md_ranges.append(md_range)

        return md_ranges, cls.filter_out_markdown_ranges(lines, md_ranges)

    @classmethod
    def find_code_ranges(cls, lines, code_prompt_regexp):
        """
        Find the ranges for the code cells.

        Works by first identifying the opening lines for code cells. Then it
        iterates over the ranges between these boundaries, breaking as soon
        as a line is not available (i.e an intermediate markdown cell).
        """
        code_starts = [(re.match(code_prompt_regexp, line), line_no)
                       for (line_no, line) in lines
                       if re.match(code_prompt_regexp, line)]
        if len(code_starts) == 0:
            return []
         # Add maximum available line number
        code_starts = code_starts + [(None, lines[-1][0])]
        available_lines = set([line_no for (line_no,_) in lines])
        code_ranges = []
        for start_info, end_info in zip(code_starts[:-1], code_starts[1:]):
            (_, end) = end_info
            (match, start) = start_info
            raw_prompt = match.groups()[0]
            try:
                prompt = int(raw_prompt) if raw_prompt not in [None, ' '] else None
            except Exception as e:
                logging.info('WARNING: Parse failure for prompt count %r' % str(e))
                prompt = None

            code_range = (start, start)
            for test_index in range(start, end+1):
                if  (test_index in available_lines):
                    code_range = (start, test_index, 'code', {'prompt':prompt})
                else:
                    code_ranges.append(code_range)
                    break
            else:
                code_ranges.append(code_range)

        return cls._adjust_code_range_boundaries(code_ranges)

    @classmethod
    def _adjust_code_range_boundaries(cls, code_ranges):
        """
        Fix boundaries between successive code cells as lines ranges
        include next prompt line otherwise.
        """
        adjusted_ranges = []
        for i, crange in enumerate(code_ranges):
            if i < (len(code_ranges)-1):
                (start, end, _, info) = crange
                next_crange = code_ranges[i+1]
                (next_start, next_end, _, _) = next_crange
                if end == next_start:
                    crange = (start, end-1, 'code', info)
            adjusted_ranges.append(crange)
        return adjusted_ranges

    @classmethod
    def collect_cells(cls, lines, ranges):
        """
        Given the set of lines and the identified cell ranges, extract the
        cells.
        """
        cells = []
        sorted_ranges = sorted(ranges)
        for (start, stop, cell_type, info) in sorted_ranges:
            source = "".join(lines[start+1:stop+1])
            md_terminator = "\n\"\"\" #:md:"
            # FIXME! Use regexp search replace when any number of newlines at end of source.
            if cell_type == "markdown" and source.endswith(md_terminator):
                source = source[:-len(md_terminator)]
            elif cell_type == "markdown" and source.endswith(md_terminator+'\n'):
                source = source[:-(len(md_terminator)+1)]
            elif ((stop != (len(lines)-1))
                  and cell_type == "code" and source.endswith("\n\n")):
                source = source[:-2]

            cells.append(dict({'mode':cell_type,
                               'source':source}, **info))
        return cells

    @classmethod
    def unique_prompts(cls, cells):
        "Ensures prompts are either unique or None"
        unique, seen = [], []
        for cell in cells:
            if cell['mode'] != 'code':
                unique.append(cell)
                continue
            elif cell['prompt'] in seen:
                cell['prompt'] = None
            else:
                seen.append(cell['prompt'])
            unique.append(cell)
        return unique

    @classmethod
    def extract_cells(cls, source, cell_cls, unique_prompts=True): # FIX cell_cls ARG
        lines =source.splitlines(keepends=True)
        md_ranges, remaining_lines = cls.find_markdown_ranges(lines)
        code_ranges = cls.find_code_ranges(remaining_lines, cell_cls.code_prompt_regexp)
        extracted = cls.collect_cells(lines, md_ranges+code_ranges)
        return cls.unique_prompts(extracted) if unique_prompts else extracted


if __name__ == '__main__':
    print(ParseNotebook.extract_cells(source, None))
