unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DelphiTwain, DelphiTwain_VCL;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAcquire: TButton;
    btnSave: TButton;
    imgHolder: TImage;
    srcLabel: TLabel;
    procedure btnAcquireClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Idle;
    procedure imgHolderPictureChanged(Sender: TObject);
  private
    Twain: TDelphiTwain;

    procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
  public

  end;

var
  Form1: TForm1;
  Status: Boolean;

implementation

{$R *.lfm}

uses my_utils;

procedure Tform1.Idle;
begin
  imgHolder.Picture.Assign(nil);
  srcLabel.Visible := True;
  imgHolder.AutoSize := True;
  btnSave.Visible := False;
  if Status then begin
    srcLabel.Caption := '';
  end;
end;

procedure TForm1.imgHolderPictureChanged(Sender: TObject);
begin
  btnSave.Visible := True;
  imgHolder.Refresh;
end;

procedure TForm1.btnAcquireClick(Sender: TObject);
begin
  //Create Twain
  if Twain = nil then begin
    Twain := TDelphiTwain.Create;
    Twain.OnTwainAcquire := @TwainTwainAcquire;
  end;

  //Load Twain Library dynamically
  if Twain.LoadLibrary then
  begin
    //Load source manager
    Twain.SourceManagerLoaded := True;

    //Allow user to select source -> only the first time
    if not Assigned(Twain.SelectedSource) then begin
      Twain.SelectSource;
      srcLabel.Caption := Twain.SelectedSource.ProductName;
    end;
    if Assigned(Twain.SelectedSource) then begin
      //Load source, select transference method and enable (display interface)}
      Twain.SelectedSource.Loaded := TRUE;
      Twain.SelectedSource.ShowUI := False;//display interface
      Twain.SelectedSource.Enabled := True;
    end;
  end else begin
    ShowMessage('Twain is not installed.');
  end;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  currentFolderPath: String;
begin
  currentFolderPath := my_CreateFolder;
  try
    imgHolder.Picture.SaveToFile(my_CreateFileName(currentFolderPath));
  finally
    Idle;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Idle;
end;

procedure TForm1.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  imgHolder.Picture.Bitmap.Assign(Image);
  Cancel := True;//Only want one image
end;

end.

