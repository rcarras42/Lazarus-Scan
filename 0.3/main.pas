unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DelphiTwain, DelphiTwain_VCL, LazFileUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAcquire: TButton;
    btnSave: TButton;
    btnExit: TButton;
    imgHolder: TImage;
    listBox: TListBox;
    srcLabel: TLabel;
    procedure btnAcquireClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Idle;
    procedure imgHolderPictureChanged(Sender: TObject);
  private
    CWD: String;
    Twain: TDelphiTwain;

    procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
      Image: TBitmap; var Cancel: Boolean);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses my_utils;

procedure Tform1.Idle;
begin
  imgHolder.Picture.Assign(nil);
  imgHolder.Proportional := True;
  btnSave.Visible := False;
  if listBox.Items.Count <> 0 then
    listBox.Visible := True
    else
      listBox.Visible := False;
end;

procedure TForm1.imgHolderPictureChanged(Sender: TObject);
begin
  btnSave.Visible := True;
  imgHolder.Refresh;
end;

procedure TForm1.btnAcquireClick(Sender: TObject);
begin
  Idle;
  try
    if not Twain.Source[Twain.SelectedSource.Index].Loaded then
      Twain.Source[Twain.SelectedSource.Index].Loaded := True;
  finally
    Twain.Source[Twain.SelectedSource.Index].ShowUI := False;
    Twain.Source[Twain.SelectedSource.Index].Enabled := True;
  end;
end;

procedure TForm1.btnExitClick(Sender: TObject);
begin
  imgHolder.Free;
  Twain.SourceManagerLoaded := False;
  listBox.Free;
  Close;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  fileName: String;
begin
  fileName := my_CreateFileName(CWD);
  try
    imgHolder.Picture.SaveToFile(fileName);
  finally
    listBox.Items.Add(ExtractFileNameWithoutExt(fileName));
    Idle;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  assetDir: String;
begin
  // Clearing assets directory before use
  my_ClearDir(Concat(GetCurrentDir, PathDelim,'assets', PathDelim));
  // Create CWD
  CWD := my_CreateFolder(Concat(GetCurrentDir, PathDelim,'assets', PathDelim));
  // Create Twain
  if Twain = nil then begin
    Twain := TDelphiTwain.Create;
    Twain.OnTwainAcquire := @TwainTwainAcquire;
  end;

  if not Twain.LoadLibrary then
    Close;

  try
    //Load source manager
    Twain.SourceManagerLoaded := True;
    Twain.SelectSource;                                                          // Have to be replaced with a default select on Twain.Source[0] and a scrollbar if multiples scanners are available on the network
    Twain.SelectedSource.Loaded := True;
  finally                                                                        // Keep in mind
    if Twain.Source[Twain.SelectedSource.Index].Loaded then begin                // Here Twain.SelectedSource.Loaded returns True even if the scanner is unpowered cuz the Source is Loaded (drivers are installed and up & runnin)
      srcLabel.Caption := Twain.SelectedSource.ProductName;                      // Were Twain.Source[SelectedSource.Index].Loaded is not
      btnAcquire.Enabled := True;
    end;
  end;
  srcLabel.Visible := True;
  Idle;
end;

procedure TForm1.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  imgHolder.Picture.Bitmap.Assign(Image);
  Cancel := True;//Only want one image
  //Twain.SelectedSource.UnloadSource;                                           // Pretty sure Cancel does that already but..
end;

end.

