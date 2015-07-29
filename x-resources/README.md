# Emacs X Resources

For a complete list of configuration keys and values check:

http://www.gnu.org/software/emacs/manual/html_mono/emacs.html#X-Resources

I prefer to put the font, toolbar and scrollbar configuration as system specific preferences. This allows emacs to start faster and there is no flicker as the features are turned on by default and then disable in the init file.

## Windows

Double click the .reg file to add settings to registry.

## OSX

Copy the .plist to ~/Library/Preferences/ and run:

  $ defaults read ~/Library/Preferences/org.gnu.Emacs

Emacs should pickup the updated preferences on the next start.

## Linux

TODO
