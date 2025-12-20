// @davidberneda August 2021
unit Unit_Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ListBox,

  Vidi_Game, FMX.Layouts;

type
  TFormSettings = class(TForm)
    CBSounds: TCheckBox;
    CBLockRotation: TCheckBox;
    CBLanguage: TComboBox;
    Rectangle1: TRectangle;
    Image2: TImage;
    Image3: TImage;
    Layout1: TLayout;
    Image1: TImage;
    Version: TText;
    ImageLanguage: TImage;
    ImageUnLock: TImage;
    ImageLock: TImage;
    ImageSoundON: TImage;
    ImageSoundOFF: TImage;
    Layout2: TLayout;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBLockRotationChange(Sender: TObject);
    procedure CBSoundsChange(Sender: TObject);
    procedure CBLanguageChange(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure ImageSoundONClick(Sender: TObject);
    procedure ImageUnLockClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Image2Click(Sender: TObject);
  private
    { Private declarations }

    Blocks : TBlocks;

    procedure CloseSettings;
    procedure SetLockImage;
    procedure SetSettings(const ABlocks:TBlocks);
    procedure SetSoundImage;
    procedure Translate;
  public
    { Public declarations }

    class procedure Modal(const AOwner:TComponent;
       const ABlocks:TBlocks;
       const AResult:TProc<TModalResult>); static;
  end;

implementation

{$R *.fmx}

uses
  Unit_Texts,
  Vidi_FMX,
  FMX.Skia,
  FMX.Platform;

procedure TFormSettings.CBLanguageChange(Sender: TObject);
begin
  case CBLanguage.ItemIndex of
    0: Blocks.Language:='ca';
    2: Blocks.Language:='es';
  else
    Blocks.Language:='en';
  end;

  SetLanguage(Blocks.Language);

  Translate;
end;

procedure TFormSettings.CBLockRotationChange(Sender: TObject);
const
  AllOrientations:TScreenOrientations=[
                     TScreenOrientation.Portrait,
                     TScreenOrientation.Landscape,
                     TScreenOrientation.InvertedPortrait,
                     TScreenOrientation.InvertedLandscape];

var
  ScreenService: IFMXScreenService;
  OrientSet: TScreenOrientations;
begin
  Blocks.LockRotation:=CBLockRotation.IsChecked;

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenService)) then
  begin
    if Blocks.LockRotation then
       OrientSet := [ScreenService.GetScreenOrientation]
    else
       OrientSet := AllOrientations;

    ScreenService.SetSupportedScreenOrientations(OrientSet);
  end;

  SetLockImage;
end;

procedure TFormSettings.SetLockImage;
begin
  if CBLockRotation.IsChecked then
  begin
    ImageLock.Position.X:=ImageLanguage.Position.X;
    ImageUnLock.Visible:=False;
    ImageLock.Visible:=True;
  end
  else
  begin
    ImageUnLock.Position.X:=ImageLanguage.Position.X;
    ImageLock.Visible:=False;
    ImageUnLock.Visible:=True;
  end;
end;

procedure TFormSettings.SetSoundImage;
begin
  if CBSounds.IsChecked then
  begin
    ImageSoundON.Position.X:=ImageLanguage.Position.X;
    ImageSoundOFF.Visible:=False;
    ImageSoundON.Visible:=True;
  end
  else
  begin
    ImageSoundOFF.Position.X:=ImageLanguage.Position.X;
    ImageSoundON.Visible:=False;
    ImageSoundOFF.Visible:=True;
  end;
end;

procedure TFormSettings.CBSoundsChange(Sender: TObject);
begin
  Blocks.Silent:=not CBSounds.IsChecked;
  SetSoundImage;
end;

procedure TFormSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=TCloseAction.caFree;
end;

procedure TFormSettings.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=vkEscape then
     Close;
end;

procedure TFormSettings.CloseSettings;
begin
  Blocks.PlaySound('click');
  Close;
end;

procedure TFormSettings.Image1Click(Sender: TObject);
begin
  CloseSettings;
end;

procedure TFormSettings.Image2Click(Sender: TObject);
begin
  CloseSettings;
end;

procedure TFormSettings.ImageSoundONClick(Sender: TObject);
begin
  CBSounds.IsChecked:=not CBSounds.IsChecked;
  SetSoundImage;
end;

procedure TFormSettings.ImageUnLockClick(Sender: TObject);
begin
  CBLockRotation.IsChecked:=not CBLockRotation.IsChecked;
  SetLockImage;
end;

procedure TFormSettings.SetSettings(const ABlocks:TBlocks);

  function IndexOfLanguage(const ALanguage:String):Integer;
  begin
    if ALanguage='ca' then
       result:=0
    else
    if ALanguage='es' then
       result:=2
    else
       result:=1;
  end;

begin
  Blocks:=ABlocks;

  CBSounds.IsChecked:=not ABlocks.Silent;
  SetSoundImage;

  CBLockRotation.IsChecked:=ABlocks.LockRotation;
  SetLockImage;

  if ABlocks.Language='' then
     CBLanguage.ItemIndex:=IndexOfLanguage(Vidi_DefaultLanguage)
  else
     CBLanguage.ItemIndex:=IndexOfLanguage(ABlocks.Language);

  Translate;
end;

class procedure TFormSettings.Modal(const AOwner:TComponent;
        const ABlocks:TBlocks; const AResult:TProc<TModalResult>);

var tmp : TFormSettings;
begin
  tmp:=TFormSettings.Create(AOwner);
  tmp.SetSettings(ABlocks);
  tmp.ShowModal(AResult);
end;

procedure TFormSettings.Translate;
var S : String;
begin
  CBSounds.Text:=Vidi_Text_Sounds;
  CBLockRotation.Text:=Vidi_Text_LockRotation;

  S:=Vidi_Text_Version+': '+Vidi_Block_Version;

  {$IFDEF CPU32BITS}
  S:=S+' 32bit';
  {$ELSE}
  {$IFDEF CPU64BITS}
  S:=S+' 64bit';
  {$ENDIF}
  {$ENDIF}

  if GlobalUseSkia then
     S:=S+' Skia';

  Version.Text:=S;

  Caption:=Vidi_Text_Settings;
end;

end.
