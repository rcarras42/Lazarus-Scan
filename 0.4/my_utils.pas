unit my_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fgl;

Type
  TIntegerList = specialize TFPGList<Integer>;

// File/Folder management
procedure my_ClearDir(const dir: String);
function my_CreatePathName(const base: String): String;
function my_CreateFolder(const base: String): String;
function my_CreateFileName(const folderPath: String): String;
function my_Archive(const src: String): Boolean;

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

function my_Archive(const src: String): Boolean;
var
  ageList:      TIntegerList;
  histPathName: String;
  srcTmp:       TStringList;
  dstTmp:       TStringList;
  dst:          String;
  i:            Integer;
begin
  histPathName := Concat(GetCurrentDir, PathDelim, 'history');
  if not DirectoryExists(histPathName) then
    CreateDir(histPathName);
  dstTmp := FindAllDirectories(histPathName);
  if dstTmp.Count > 5 then begin
  {  for i := dstTmp.Count downto 0 do
      ageList.Items.Add(FileAge(dstTmp[i]));
    // Sort by age and remove the oldest one then copy src here with deleted item's filename
    end;        }
  end else begin
    srcTmp := FindAllFiles(src, '*.png');
    if srcTmp.Count > 0 then begin
      for i := srcTmp.Count downto 0 do
        dst := Concat(
          histPathName, PathDelim, IntToStr(dstTmp.Count + 1), PathDelim, ExtractFilename(srcTmp[i]));
        CopyFile(srcTmp[i], dst);
      end;
    end;

    FreeAndNil(dstTmp);
    dstTmp := FindAllFiles(dst, '*.png');
    if dstTmp.Count <> srcTmp.Count then
      Result := False
    else begin
      Result := True;
      DeleteDirectory(src);
    end;
  end;
  srcTmp.Free;
  dstTmp.Free;
  ageList.Free;
end.



