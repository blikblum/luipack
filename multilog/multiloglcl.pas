unit multiloglcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, multilog;

type

  { TLCLLogger }

  TLCLLogger = class(TLogger)
  private
  public
    procedure SendBitmap(const AText: String; ABitmap: TBitmap); //inline;
    procedure SendBitmap(Classes: TDebugClasses; const AText: String; ABitmap: TBitmap);
    procedure SendColor(const AText: String; AColor: TColor); //inline;
    procedure SendColor(Classes: TDebugClasses; const AText: String; AColor: TColor);
  end;
  
  function ColorToStr(Color: TColor): String;

implementation


function ColorToStr(Color: TColor): String;
begin
  case Color of

    clBlack   : Result:='clBlack';
    clMaroon  : Result:='clMaroon';
    clGreen   : Result:='clGreen';
    clOlive   : Result:='clOlive';
    clNavy    : Result:='clNavy';
    clPurple  : Result:='clPurple';
    clTeal    : Result:='clTeal';
    clGray {clDkGray}   : Result:='clGray/clDkGray';
    clSilver {clLtGray}  : Result:='clSilver/clLtGray';
    clRed     : Result:='clRed';
    clLime    : Result:='clLime';
    clYellow  : Result:='clYellow';
    clBlue    : Result:='clBlue';
    clFuchsia : Result:='clFuchsia';
    clAqua    : Result:='clAqua';
    clWhite   : Result:='clWhite';
    clCream   : Result:='clCream';
    clNone    : Result:='clNone';
    clDefault : Result:='clDefault';
    clMoneyGreen : Result:='clMoneyGreen';
    clSkyBlue    : Result:='clSkyBlue';
    clMedGray    : Result:='clMedGray';
    clScrollBar               : Result:='clScrollBar';
    clBackground              : Result:='clBackground';
    clActiveCaption           : Result:='clActiveCaption';
    clInactiveCaption         : Result:='clInactiveCaption';
    clMenu                    : Result:='clMenu';
    clWindow                  : Result:='clWindow';
    clWindowFrame             : Result:='clWindowFrame';
    clMenuText                : Result:='clMenuText';
    clWindowText              : Result:='clWindowText';
    clCaptionText             : Result:='clCaptionText';
    clActiveBorder            : Result:='clActiveBorder';
    clInactiveBorder          : Result:='clInactiveBorder';
    clAppWorkspace            : Result:='clAppWorkspace';
    clHighlight               : Result:='clHighlight';
    clHighlightText           : Result:='clHighlightText';
    clBtnFace                 : Result:='clBtnFace';
    clBtnShadow               : Result:='clBtnShadow';
    clGrayText                : Result:='clGrayText';
    clBtnText                 : Result:='clBtnText';
    clInactiveCaptionText     : Result:='clInactiveCaptionText';
    clBtnHighlight            : Result:='clBtnHighlight';
    cl3DDkShadow              : Result:='cl3DDkShadow';
    cl3DLight                 : Result:='cl3DLight';
    clInfoText                : Result:='clInfoText';
    clInfoBk                  : Result:='clInfoBk';
    clHotLight                : Result:='clHotLight';
    clGradientActiveCaption   : Result:='clGradientActiveCaption';
    clGradientInactiveCaption : Result:='clGradientInactiveCaption';
    clForm                    : Result:='clForm';
    clEndColors               : Result:='clEndColors';
    {
    //todo find the conflicts
    clColorDesktop            : Result:='clColorDesktop';
    cl3DFace                  : Result:='cl3DFace';
    cl3DShadow                : Result:='cl3DShadow';
    cl3DHiLight               : Result:='cl3DHiLight';
    clBtnHiLight              : Result:='clBtnHiLight';
    }
  else
    Result:= 'Unknow Color';
  end;//case
  Result:=Result+' ($'+IntToHex(Color,6)+')';
end;

{ TLCLLogger }

procedure TLCLLogger.SendBitmap(const AText: String; ABitmap: TBitmap);
begin
  SendBitmap(DefaultClasses,AText,ABitmap);
end;

procedure TLCLLogger.SendBitmap(Classes: TDebugClasses; const AText: String;
  ABitmap: TBitmap);
var
  AStream: TStream;
begin
  if Classes * ActiveClasses = [] then Exit;
  if ABitmap <> nil then
  begin
    AStream:=TMemoryStream.Create;
    ABitmap.SaveToStream(AStream);
    //hack to avoid keeping a copy in ABitmap
    ABitmap.Canvas.Changing;
    ABitmap.Canvas.Changed;
  end
  else
    AStream:=nil;
  //SendStream free AStream
  SendStream(ltBitmap,AText,AStream);
end;

procedure TLCLLogger.SendColor(const AText: String; AColor: TColor);
begin
  SendColor(DefaultClasses,AText,AColor);
end;

procedure TLCLLogger.SendColor(Classes: TDebugClasses; const AText: String;
  AColor: TColor);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(ltValue,AText+' = '+ColorToStr(AColor),nil);
end;

end.

