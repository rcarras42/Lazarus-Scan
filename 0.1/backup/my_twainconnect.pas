unit my_TwainConnect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DelphiTwain, DelphiTwain_VCL;

private
  Twain: TDelphiTwain;

  procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
    Image: TBitmap; var Cancel: Boolean);

implementation

function my_TwainCreate;
begin
  if Twain
end;

end.

