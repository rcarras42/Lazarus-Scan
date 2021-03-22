unit connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DelphiTwain, DelphiTwain_VCL, ExtCtrls;

type

  ImageHolder: TImage;
  procedure Button1Click(Sender: TObject);
  private
    Twain: TDelphiTwain;

    procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
    end;

implementation

procedure TForm1.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  ImageHolder.Picture.Bitmap.Assign(Image);
  Cancel := True;//Only want one image
end;

end.

