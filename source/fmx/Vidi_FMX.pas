unit Vidi_FMX;

interface

uses
  FMX.Objects;

procedure AdjustFontSize(const AText:TText; const AMax:Single);
function IsDarkMode: Boolean;
function Vidi_DefaultLanguage:String;

implementation

uses
  FMX.Graphics, FMX.Controls, FMX.Platform, System.SysUtils;

procedure AdjustFontSize(const AText:TText; const AMax:Single);
var Limit,
    tmp : Single;
begin
  AText.Font.Size:=AMax;

  Limit:=AText.Width*0.7;

  AText.Canvas.BeginScene;
  try
    repeat
      AText.Canvas.Font.Size:=AText.Font.Size;

      tmp:=AText.Canvas.TextWidth(AText.Text);

      if tmp>=Limit then
         AText.Font.Size:=AText.Font.Size-0.5;

    until tmp<Limit;
  finally
    AText.Canvas.EndScene;
  end;
end;

function IsDarkMode: Boolean;
var tmp : IFMXSystemAppearanceService;
begin
  result:=False;

  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemAppearanceService, tmp) then
     result:=tmp.GetSystemThemeKind=TSystemThemeKind.Dark;
end;

function Vidi_DefaultLanguage:String;
var
  LocServ: IFMXLocaleService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(LocServ)) then
     result := LowerCase(Copy(LocServ.GetCurrentLangID,1,2))
  else
     result := '';
end;

end.
