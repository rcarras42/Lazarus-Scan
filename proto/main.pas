unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtDlgs,
  StdCtrls, ExtCtrls, ComCtrls, LazFileUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSaveOne: TButton;
    btnSaveAll: TButton;
    btnClear: TButton;
    btnExit: TButton;
    ImageHolder: TImage;
    mnuQuit: TMenuItem;
    OpenBox: TListBox;
    MainMenu: TMainMenu;
    Document: TMenuItem;
    mnuOpen: TMenuItem;
    mnuNew: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure SetIdle;
    procedure btnClearClick(Sender: TObject);
    procedure btnExitNoClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSaveAllClick(Sender: TObject);
    procedure btnSaveOneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure OpenBoxSelectionChange(Sender: TObject; User: boolean);

  private

  public

end;
var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SetIdle;
begin
  ImageHolder.Picture.Assign(nil);
  ImageHolder.Visible := False;

  OpenBox.Visible := False;

  btnClear.Visible := False;
  btnSaveOne.Visible := False;
  btnSaveAll.Visible := False;

  btnExit.Visible := False;
end;

procedure TForm1.mnuOpenClick(Sender: TObject);
var
  Filename: string;
  ExtractedFilename: string;

begin
   if OpenPictureDialog1.Execute then begin
     Filename := OpenPictureDialog1.Filename;
     ExtractedFilename := ExtractFilenameOnly( Filename );
     OpenBox.Items.Add( ExtractedFilename );
     OpenBox.Visible := True;
     ImageHolder.Picture.LoadFromFile( Filename );
     ImageHolder.Visible := True;
     btnSaveOne.Visible := True;
     btnSaveAll.Visible := True;
     btnClear.Visible := True;
   end;
end;


procedure TForm1.OpenBoxSelectionChange(Sender: TObject; User: boolean);
begin
  ImageHolder.Picture.Assign(nil);
  ImageHolder.Picture.LoadFromFile(
  Concat(GetCurrentDir, '/resource/', OpenBox.GetSelectedText, '.png'));
  ImageHolder.Refresh;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetIdle;
end;

procedure TForm1.btnSaveOneClick(Sender: TObject);
begin
  ShowMessage('Ask Identifier for customer.' +
  #13#10 + ' What to do with that picture ?');
end;

procedure TForm1.btnSaveAllClick(Sender: TObject);
begin
  ShowMessage('Same as SaveOne, but the whole list will be pushed');
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  SetIdle;
end;

procedure TForm1.btnExitNoClick(Sender: TObject);
begin
  SetIdle;
end;

procedure TForm1.btnExitClick(Sender: TObject);
begin
  ImageHolder.Free;
  Close;
end;

end.

