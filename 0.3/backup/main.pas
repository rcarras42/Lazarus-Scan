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
    lblSource: TLabel;
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
    ACQUIRED: ShortInt;
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
  // Reset imgHoolder container
  imgHolder.Picture.Assign(nil);
  imgHolder.Proportional := True;
  // Save and delete are only visible if a picture is loaded.
  btnSave.Visible := False;
  btnDeleteImage.Visible := False;
  // Update buttons's visibility according to our listBox
  if listBox.Items.Count > 0 then begin
    listBox.Visible := True;
    btnValidate.Visible := True;
    end else begin
      listBox.Visible := False;
      btnValidate.Visible := False;
    end;
end;


procedure TForm1.listBoxSelectionChange(Sender: TObject; User: boolean);
begin
  // Set mode to NOT PREVIEW (uses in Delete image procedure)
  PREVIEW := False;
  // Set IMAGEPATH according to our selection in the list (uses in Save/Delete image procedure)
  IMAGEPATH := Concat(
  CWD, pathDelim, listBox.GetSelectedText, '.png');
  // Update imgHolder with our selected item
  imgHolder.Picture.Assign(nil);
  imgHolder.Picture.LoadFromFile(IMAGEPATH);
  // Update utility button(s)'s visibility
  btnDeleteImage.Visible := True;

  imgHolder.Refresh;
end;

procedure TForm1.btnAcquireClick(Sender: TObject);
begin
  // Reset imgHolder
  Idle;
  // Load Twain
  Twain.SelectedSource.Loaded := True;
  // Disable UI
  Twain.SelectedSource.ShowUI := False;
  // Acquire
  Twain.SelectedSource.Enabled := True;

  // Set mode to PREVIEW
  PREVIEW := True;

  // Pop out primary control buttons
  btnSave.Visible := True;
  btnDeleteImage.Visible := True;

  // Debug purpose only
  ACQUIRED := ACQUIRED + 1;
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
    // Load source here even if it's unecessary because we want to know the status of the scanner
    Twain.SelectedSource.Loaded := True;
  finally                                                                        // Keep in mind
    if Twain.Source[Twain.SelectedSource.Index].Loaded then begin                // Here Twain.SelectedSource.Loaded returns True even if the scanner is unpowered cuz the Source is Loaded (drivers are installed and up & runnin)
      lblSource.Caption := Twain.SelectedSource.ProductName;                      // Were Twain.Source[SelectedSource.Index].Loaded is not
      btnAcquire.Enabled := True;
    end;
  end;
  lblSource.Visible := True;
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

