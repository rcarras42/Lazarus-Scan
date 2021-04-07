unit my_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

// File/Folder management
procedure my_ClearDir(const dir: String);
function my_CreatePathName(const base: String): String;
function my_CreateFolder(const base: String): String;
function my_CreateFileName(const folderPath: String): String;

implementation

procedure my_ClearDir(const dir: String);
var
  i: ShortInt;
  dirs: TStringList;
begin
  dirs := FindAllDirectories(dir, False);
  if dirs.Count > 1 then begin                                                    // A bit of cleaning
    for i := (dirs.Count - 1) downto 0 do                                         // Keep last one(just in case...)
        DeleteDirectory(dirs[i],False);                                           // Will have to swap 0 & 1 here otherwise 0 will always be the first created and never modified
  end;
  dirs.Free;
end;

function my_CreatePathName(const base: String): String;
var
  dirs: TStringList;
begin
  if not DirectoryExists(base) then
    CreateDir(base);

  dirs := FindAllDirectories(base, False);
  my_CreatePathName := Concat(base ,IntToStr(dirs.Count));
  dirs.Free;
end;

function my_CreateFolder(const base: String): String;
var
  cwd: String;
begin
  cwd := my_CreatePathName(base);
  if not DirectoryExists(cwd) then
    CreateDir(cwd);
    if DirectoryExists(cwd) then
      my_CreateFolder := cwd;
end;

function my_CreateFileName(const folderPath: String): String;
var
  tmp: TStringList;
begin
  tmp := FindAllFiles(folderPath, '*.png', True);
  try
    my_CreateFileName := Concat(
    folderPath, PathDelim, 'scan_', IntToStr(tmp.Count + 1), '.png' );
  finally
    tmp.Free;
  end;
end;


end.

