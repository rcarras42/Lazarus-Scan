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
//function my_Archive(const src: String): Boolean;

implementation

procedure my_ClearDir(const dir: String);
var
  i: ShortInt;
  dirs: TStringList;
begin
  dirs := FindAllDirectories(dir, False);
  if dirs.Count > 1 then begin                                                    
    for i := (dirs.Count - 1) downto 0 do                                         
        DeleteDirectory(dirs[i],False);                                           
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
{
function my_Archive(const src: String): Boolean;
var
  histPathName: String;
  srcTmp:       TStringList;
  dstTmp:       TStringList;
  st, dst:      String;
begin
  histPathName := Concat(GetCurrentDir, PathDelim, 'history');
  if not DirectoryExists(histPathName) then
    CreateDir(histPathName);
  dstTmp := FindAllDirectories(histPathName);
  if dstTmp.Count > 5 then begin
    for i := dstTmp.Count downto 0 do
      ageList.Items.Add(FileAge(dstTmp[i]));
    // Sort by age and remove the oldest one then copy src here with deleted item's filename
    end;        
  end else begin
    srcTmp := FindAllFiles(src, '*.png');
    if srcTmp.Count > 0 then
      for st in srcTmp do begin
        dst := histPathName + PathDelim + IntToStr(dstTmp.Count + 1) + PathDelim + ExtractFilename(st);
        CopyFile(st, dst);
      end;

    FreeAndNil(dstTmp);
    dstTmp := FindAllFiles(dst, '*.png');
    Result := (dstTmp.Count = srcTmp.Count);
    if Result then
      DeleteDirectory(src, False);
  end;
  srcTmp.Free;
  dstTmp.Free;
end;
}
end.



