#!/Applications/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak -headless /Applications/Squeak6.0-22148-64bit.app/Contents/Resources/Squeak6.0-22148-64bit.image

"1. Use the actual OS process stdout"
FileStream stdout nextPutAll: 'Hello from the terminal!'; cr.
FileStream stdout flush.

"2. Force an immediate exit of the VM process"
Smalltalk snapshot: false andQuit: true.


