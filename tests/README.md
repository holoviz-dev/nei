## Testing

Currently implements integration tests running on Travis. Testing
infrastructure is currently a work in progress.

One thing that causes complication is that I have not found a source of
Emacs builds on OSX that are simple archives containing
executables. Instead, what seems to work is downloading from
[emacsformacosx](https://emacsformacosx.com) and loading the dmg file as
follows:


```shell
wget https://emacsformacosx.com/emacs-builds/Emacs-26.1-universal.dmg
hdiutil attach Emacs-26.1-universal.dmg
# Using /Volumes/Emacs/Emacs.app/Contents/MacOS/Emacs as the executable here
hdiutil detach /Volumes/Emacs
```

Will consider using doit or a MakeFile to help automate this process to
make NEI easily testable on Linux and OSX.
