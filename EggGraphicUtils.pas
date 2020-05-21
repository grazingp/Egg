unit EggGraphicUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, ImgList, types, LCLType, contnrs,
  Clipbrd, LCLIntf, Forms, StdCtrls, ExtCtrls, Controls, IntfGraphics, FPimage
  ;

// Resize Create PNG
function EggCreatePNG(APicture: TPicture; AWidth, AHeight: Integer): TPortableNetworkGraphic;
procedure EggFillTransparent(AGraphic: TPortableNetworkGraphic);

implementation

function EggCreatePNG(APicture: TPicture; AWidth, AHeight: Integer): TPortableNetworkGraphic;
begin
	Result := TPortableNetworkGraphic.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(AWidth, AHeight);
  EggFillTransparent(Result);
  Result.Canvas.StretchDraw(Rect(0, 0, AWidth, AHeight), APicture.Graphic);
end;

procedure EggFillTransparent(AGraphic: TPortableNetworkGraphic);
var
  t: TLazIntfImage;
  i, j: Integer;
begin
  t := nil;
  try
    t := AGraphic.CreateIntfImage;
    for i := 0 to Pred(t.Width) do
    begin
    	for j := 0 to Pred(t.Height) do
      begin
        t.Colors[i, j] := colTransparent;
      end;
    end;
    AGraphic.LoadFromIntfImage(t);
  finally
    FreeAndNil(t);
  end;
end;

end.

