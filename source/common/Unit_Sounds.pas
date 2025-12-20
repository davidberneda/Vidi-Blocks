// @davidberneda August 2021
unit Unit_Sounds;

interface

{$IFNDEF MSWINDOWS}
uses
  FMX.Media;
{$ENDIF}

var Sound_Folder:String;
    Sound_Extension:String;
    Sound_UseMedia:Boolean=True;

    {$IFNDEF MSWINDOWS}
    Sound_MediaPlayer : TMediaPlayer;
    {$ENDIF}

procedure Vidi_PlaySound(const AName:String);

{$IFDEF MSWINDOWS}
procedure Vidi_PlaySound_Resource(const AName:String);
{$ENDIF}

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, MMSystem,
  {$ENDIF}

  {$IFOPT D+}
  System.SysUtils,
  {$ENDIF}

  System.IOUtils;

{$IFNDEF MSWINDOWS}
procedure PlaySoundCrossPlatform(const AFile:String);
begin
  Sound_MediaPlayer.FileName:=AFile;
  Sound_MediaPlayer.Play;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure Vidi_PlaySound_Resource(const AName:String);
var HResInfo: THandle;
    HGlobal: THandle;
    P : Pointer;
begin
  HResInfo:=FindResource(HInstance, PWideChar(AName), 'WAVE');

  {$IFOPT D+}
  if HResInfo=0 then
     raise Exception.CreateFmt('Cannot find resource %s', [AName]);
  {$ENDIF}

  HGlobal :=LoadResource(HInstance, HResInfo);
  try
    {$IFOPT D+}
    if HGlobal = 0 then
       raise Exception.CreateFmt('Cannot load resource %s', [AName]);
    {$ENDIF}

    P:=LockResource(HGlobal);
    try
      sndPlaySound(P, SND_MEMORY or SND_ASYNC or SND_NODEFAULT);
    finally
      UnlockResource(HGlobal);
    end;
  finally
    FreeResource(HGlobal);
  end;
end;
{$ENDIF}

procedure Vidi_PlaySound(const AName:String);
var S : String;
begin
  {$IFDEF MSWINDOWS}
  S:=TPath.Combine(Sound_Folder,AName+Sound_Extension);

  {$IFOPT D+}
  if not TFile.Exists(S) then
     raise Exception.Create('Missing file: '+S);
  {$ENDIF}

  {
  if Sound_UseMedia then
     PlaySoundCrossPlatform(S)
  else
  }
     sndPlaySound(PWideChar(S),SND_NODEFAULT or SND_ASYNC);
  {$ELSE}

  S:=TPath.Combine(TPath.GetHomePath,AName+'.mp3');

  {$IFOPT D+}
  if not TFile.Exists(S) then
     raise Exception.Create('Missing file: '+S);
  {$ENDIF}

  PlaySoundCrossPlatform(S);
  {$ENDIF}
end;

procedure Sound_SetupParameters;
begin
  {$IFDEF MSWINDOWS}
  Sound_Folder:='';

  Sound_Extension:='.wav';
  Sound_UseMedia:=False;
  {$ELSE}
  Sound_Folder:=TPath.GetHomePath;

  Sound_Extension:='.mp3';
  Sound_UseMedia:=True;
  {$ENDIF}
end;

initialization
  Sound_SetupParameters;
end.
