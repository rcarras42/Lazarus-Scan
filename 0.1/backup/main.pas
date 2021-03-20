unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAcquire: TButton;
    Image1: TImage;
    procedure btnAcquireClick(Sender: TObject);

  protected

  private

  public
  end;

var
  Form1: TForm1;

implementation

uses my_utils;
{$R *.lfm}

{ TForm1 }


procedure TForm1.btnAcquireClick(Sender: TObject);
var
  myDir: String;
begin
  myDir := '';
  // Lets' get a working directory to deal with
  if (myDir = '') then
    myDir := my_CreateFolder;
  ShowMessage(myDir);

  // Get img with my_GetImageFromTwain
end;

end.

