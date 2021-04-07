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
    btnDeleteImage: TButton;
    btnValidate: TButton;
    imgHolder: TImage;
    listBox: TListBox;
    srcLabel: TLabel;
    procedure btnAcquireClick(Sender: TObject);
    procedure btnDeleteImageClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Idle;
    procedure listBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure btnValidateClick(Sender: TObject);
  private
    CWD: String;
    IMAGEPATH: String;
    PREVIEW: Boolean;
    Twain: TDelphiTwain;

    procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
      Image: TBitmap; var Cancel: Boolean);

  public
    Result: String;
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
  btnDeleteImage.Visible := False;
  if listBox.Items.Count > 0 then
    listBox.Visible := True
    else
      listBox.Visible := False;
end;


procedure TForm1.listBoxSelectionChange(Sender: TObject; User: boolean);
begin
  PREVIEW := False;
  IMAGEPATH := Concat(
  CWD, pathDelim, listBox.GetSelectedText, '.png');
  imgHolder.Picture.Assign(nil);
  imgHolder.Picture.LoadFromFile(IMAGEPATH);
  btnDeleteImage.Visible := True;
  imgHolder.Refresh;
end;

procedure TForm1.btnAcquireClick(Sender: TObject);
begin
  Idle;
  try
    if not Twain.Source[Twain.SelectedSource.Index].Loaded then
      Twain.Source[Twain.SelectedSource.Index].Loaded := True;
  finally                                                                        // Something wrong is happening here..
    Twain.Source[Twain.SelectedSource.Index].ShowUI := False;
    Twain.Source[Twain.SelectedSource.Index].Enabled := True;                    // WTF IS GOING ON HERE ??

    btnSave.Visible := True;

    PREVIEW := True;
    btnDeleteImage.Visible := True;
  end;
end;

procedure TForm1.btnDeleteImageClick(Sender: TObject);
begin
  if not PREVIEW then begin
    try
        DeleteFile(IMAGEPATH);
    finally
        if FileExists(IMAGEPATH, False) then
          ShowMessage('Check btnDeleteImageClick');
        listBox.Items.Delete(listBox.ItemIndex);
    end;
  end;
  Idle;
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
    fileName := ExtractFileNameOnly(fileName);
    listBox.Items.Add(fileName);
    Idle;
  end;
end;

procedure TForm1.btnValidateClick(Sender: TObject);
begin
  // Sending the folder's path filled with image(s) as PNG format.

  if listBox.Items.Count > 0 then
      Result := CWD
      else
          ShowMessage('Nothing to send!');
end;

procedure TForm1.FormCreate(Sender: TObject);
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

