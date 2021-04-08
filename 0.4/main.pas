unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ActnList, DelphiTwain, DelphiTwain_VCL, LazFileUtils, Twain;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAcquire:        TButton;
    btnSave:           TButton;
    btnExit:           TButton;
    btnDeleteImage:    TButton;
    btnValidate:       TButton;

    cbScanSettings:    TComboBox;
    imgHolder:         TImage;
    listBox:           TListBox;
    lblSource:         TLabel;

    procedure btnAcquireClick(Sender: TObject);
    procedure btnDeleteImageClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbScanSettingsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Idle;
    procedure listBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure btnValidateClick(Sender: TObject);
  private
    {Default 0 if only 1 scanner detected, if > 1 Twain.SelectSource is called}
    SOURCEINDEX:  ShortInt;

    {Current Working Directory, for images}
    CWD:          String;
    {Displayed image path}
    IMAGEPATH:    String;
    {View mode: PREVIEW/not PREVIEW}
    PREVIEW:      Boolean;

    {Twain component}
    Twain:        TDelphiTwain;

   {DEBUG}
    ACQUIRED:     ShortInt;

    procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
      Image: TBitmap; var Cancel: Boolean);

//    function _GetOneValue(tw: TDelphiTwain; index: Integer; const what: String): String; 
  public
    Result: String;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses my_utils;




procedure TForm1.btnValidateClick(Sender: TObject);
begin
  // Sending the folder's path filled with image(s) as PNG format.

  if listBox.Items.Count > 0 then
      Result := CWD
      else
          ShowMessage('Nothing to send!');
end;





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
  // Load Twain, Disable UI, Acquire
  Twain.Source[SOURCEINDEX].Loaded := True;
  Twain.Source[SOURCEINDEX].ShowUI := False;
  Twain.Source[SOURCEINDEX].Enabled := True;

  // Set mode to PREVIEW
  PREVIEW := True;

  // Pop out primary control buttons
  btnSave.Visible := True;
  btnDeleteImage.Visible := True;

  // Debug purpose only
  ACQUIRED := ACQUIRED + 1;
end;

procedure TForm1.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  imgHolder.Picture.Bitmap.Assign(Image);
  Cancel := True;//Only want one image
  //Source is unloaded here!
end;


// Buttons

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
var
  clear: Boolean;
begin
  imgHolder.Free;
  Twain.SourceManagerLoaded := False;
  listBox.Free;

  clear := my_Archive(CWD);
  ShowMessage(BoolToStr(clear));
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

  if not Twain.LoadLibrary then begin
    ShowMessage('Twain library not found, exiting');
    Close;
  end else begin
        Twain.SourceManagerLoaded := True;
        if not Twain.SourceManagerLoaded then begin
          ShowMessage('Cannot load Twain.SourceManager, Exiting');
          Close;
        end;
  end;

  // my_SourceManager
  try
    // Make Source[0] default
    case Twain.SourceCount of
      0: SOURCEINDEX := -1;
      1: SOURCEINDEX := 0;
    else
      Twain.SelectSource;
      SOURCEINDEX := Twain.SelectedSource.Index;
    end;

    // Connectivity check purpose only
    Twain.Source[SOURCEINDEX].Loaded := True;
  finally
    if SOURCEINDEX >= 0 then begin
      if Twain.Source[SOURCEINDEX].Loaded then begin
        lblSource.Caption := Twain.Source[SOURCEINDEX].ProductName;
        btnAcquire.Enabled := True;
      end;
    end else begin
            ShowMessage('Scanner unpowered ?, exiting (this will change)');
            Close;
    end;
  end;
  lblSource.Visible := True;

  //ShowMessage(IntToStr(ICAP_XRESOLUTION)+#10#13+IntToStr(ICAP_YRESOLUTION));
  // ComboBox setup
  cbScanSettings.Items.Clear;
  cbScanSettings.Caption := 'Resolution (DotsPerInch)';
  cbScanSettings.Items.Add('150');
  cbScanSettings.Items.Add('300');
  cbScanSettings.Items.Add('600');
  Idle;
end;
 
procedure TForm1.cbScanSettingsChange(Sender: TObject);
var
  {%H-}dpi:   String;
  resolution: LongInt;
  capXResolution:       SmallInt;
begin
  case cbScanSettings.ItemIndex of
    0: dpi := '150';
    1: dpi := '300';
    2: dpi := '600';
  else
    dpi := '300';
  end;
  resolution := StrToInt(cbScanSettings.Items[cbScanSettings.ItemIndex]);
  // xCap before SetOneValue := 4731

  Twain.Source[SOURCEINDEX].SetOneValue(TWTY_FIX32,ICAP_XRESOLUTION,@resolution);
  capXResolution := ICAP_XRESOLUTION;

{
  capXResolution := Twain.Source[SOURCEINDEX].GetOneValue(
    TWTY_FIX32,TW_UINT16,IntToStr(ICAP_XRESOLUTION));


  ShowMessage('Picked: '+IntToStr(resolution)+#13#10+'capXResolution: '+IntToStr(xCap));
}
end;

end.

