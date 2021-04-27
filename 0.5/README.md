Do NOT read me!

## WIP
#### Implementation
- [ ] Basic control group for scanner settings
	- [ ] DPI
	- [ ] Transfer mode
- [ ] Capabilities settings
	- [ ] DPI
	- [ ] Transfer mode
- [ ] File management fix & implementation last track
	- [ ] Fix
	- [ ] Last track


#### Bugs
- [ ] 'scan_2' twice if 'scan_1' deleted

## File management

- [x] Create dirName
- [x] Create CWD (folder for current session)
- [x] SaveToFile
- [x] Delete CWD
- [ ] Keep last folder
	- On Quit
		- Delete `assets/0/\*.png`
		- Move `assets/1/\*.png`	`assets/0/`
	- On Open
		- List all files .png in assets/0/ in the listBox


## Twain
#### Setup
- [x] Load Library
- [x] Load SourceManager

#### Acquire
- [x] Load Source
- [x] Enable


## UI
- [ ] menuBar
- [ ] Canvas
- [ ] imageBox instead of listBox
- [x] btnExit
- [x] btnAcquire
- [x] btnSave
- [x] btnDelete
	- [x] Preview mode
	- [x] File mode
- [ ] btnEdit
	- [ ] Rotate
	- [ ] Shrink/resize
- [ ] Settings template
	- [ ] Dropdown with all sources available
	- [ ] Last DPI selected
	- [ ] Last Transfer mode selected
- [ ] Update the srcLabel in runtime 