unit my_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtCtrls;

// File/Folder management
function my_CreatePathName: String;
function my_CreateFolder: String;
function my_CreateFileName(const folderPath: String): String;

// Image related stuff
function my_SaveImageToFile(const folderPath: String; const img: TImage): Boolean;



implementation

function my_CreatePathName: String;
var
  base: String;
  dirs: TStringList;
  i: ShortInt;
begin
  base := Concat(GetCurrentDir, PathDelim,'assets', PathDelim);

  if not DirectoryExists(base) then
    CreateDir(base);

  dirs := FindAllDirectories(base, False);
  if dirs.Count > 1 then begin                                                    // A bit of sanitizing
    for i := (dirs.Count - 1) downto 1 do                                         // Keep last one(just in case...)
        DeleteDirectory(dirs[i],False);                                           // Not sure about the downto will find out soon
  end;
  my_CreatePathName := Concat(base ,IntToStr(dirs.Count));
  dirs.Free;
end;

function my_CreateFolder: String;
var
  cwd: String;
begin
  cwd := my_CreatePathName;
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

// Image Management


function my_SaveImageToFile(const folderPath: String; const img: TImage): Boolean;
var
  fileName: String;
  res: Boolean;
begin
  res := False;
  fileName := my_CreateFileName(folderPath);
  // Image.Picture.Graphic.SaveToStream
  // Create empty PNG object (TPNGObject.Create)
  // Load data from stream to png object
  if FileExists(fileName) then
    res := True;

  img.Picture.Assign(nil);
  img.Free;
  my_SaveImageToFile := res;
end;


end.

