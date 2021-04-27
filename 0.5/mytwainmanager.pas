unit MyTwainManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DelphiTwain, DelphiTwain_VCL, main;

type
	
	{ My twain manager }
	TTwainManager = class
	private
		{ Twain Component }
		Twain: TDelphiTwain;
		
		AErrorMode: String;
		AErrorMessage: String;

		ImgHolder: TBitmap;

		procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
			Image: TBitmap; var Cancel: Boolean);

		{ Write error mode for handler }
		function SetErrorMode(ErrorMode: String): String;
		{ Write error message for handler }
		function SetErrorMessage(ErrorMessage: String): String;
	public
		{ What to do after receiving error ? }
		property ErrorMode: String read AErrorMode write SetErrorMode;
		{ What kind of error we got }
		property ErrorMessage: String read AErrorMessage write SetErrorMessage;

		{ Enable connectivity with scanner }
		function InitTwain: Boolean;
	end;

implementation

function SetErrorMode(ErrorMode: String): String;
begin
	AErrorMode := ErrorMode;
end;

function SetErrorMode(ErrorMessage: String): String;
begin
	AErrorMessage := ErrorMessage;
end;

function TTwainManager.InitTwain: Boolean;
begin
	SetErrorMode('retry');
	if Twain = nil then begin
		//If Twain does not exists, then create it (Twain Component)
		Twain := TDelphiTwain.Create;
		//Link co-routine
		Twain.OnTwainAcquire := @TwainTwainAcquire;

	if not Twain.LoadLibrary then begin
		//More Handlers will come here later
		if AErrorMode = 'retry' then begin
			SetErrorMode('close');
			SetErrorMessage('Cannot load Twain Library Closing..');
		end;
	end else begin
		//Load Source Manager
		Twain.SourceManager.Loaded := True;
		if not Twain.SourceManagerLoaded then begin
			SetErrorMode('close');
			SetErrorMessage('Cannot load Twain Source Manager. Closing..');
		end;
		SetErrorMessage('Everything looks good till here.');
	end;
end;

procedure TTwainManager.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  ImgHolder.Picture.Bitmap.Assign(Image);
  Cancel := True;
end;

end.

