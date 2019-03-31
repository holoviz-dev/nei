" Very simple proof of concept showing Vim integration with NEI 
"
" Initial prototype guided with inspiration from the vim-ipython project
" (https://github.com/ivanov/vim-ipython)
"
" -----------------
" Quickstart Guide:
" -----------------
"
" Requires Vim with +python3 that has tornado available
"
" First launch the NEI server in an environment where NEI is installed:
"
" python -c "import nei;nei.serve(ws_port=9995)"
"
" In Vim:
"
"   :source nei.vim
"   :NEI
"
"
" Now you can open a NEI browser window by pressing F5.
"
" Next you can select multiple lines in visual line mode (shift+V) and
" press F6. The browser view will then update to display the selected lines.

if !has('python3')
    finish
endif

" Allow custom mappings.
if !exists('g:ipy_perform_mappings')
    let g:ipy_perform_mappings = 1
endif


python3 << EOF
import vim
import sys
vim_nei_path = vim.eval("expand('<sfile>:h')")
sys.path.append(vim_nei_path)
from vim_nei import *
EOF


" Setup mappings for running NEI from Vim
noremap  <Plug>(NEI-RunLines)            :python3 run_these_lines()<CR>
noremap  <Plug>(NEI-ViewBrowser)         :python3 view_browser()<CR>

if g:ipy_perform_mappings != 0
    map  <buffer>  <F5>           <Plug>(NEI-ViewBrowser)
    imap <buffer>  <C-F5>         <C-o><Plug>(NEI-ViewBrowser)
    imap <buffer>  <F5>           <C-o><Plug>(NEI-ViewBrowser)

    map  <buffer>  <F6>           <Plug>(NEI-RunLines)
    imap <buffer>  <C-F6>         <C-o><Plug>(NEI-RunLines)
    imap <buffer>  <F6>           <C-o><Plug>(NEI-RunLines)

    "pi custom
    map  <buffer> <silent> <C-Return>     <Plug>(NEI-RunLines)
    map  <buffer> <silent> <C-Return>     <Plug>(NEI-ViewBrowser)      
endif

command! -nargs=* NEI :py3 nei_connect_to_server("<args>")