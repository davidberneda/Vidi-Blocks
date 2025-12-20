// @davidberneda August 2021
{$SCOPEDENUMS ON}
unit Unit_Blocks;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Media,

  Vidi_Game, Vidi_Graphics, Unit_Game, Vidi_Canvas;

type
  TFormMain = class(TForm)
    Main: TPaintBox;
    TopLayout: TLayout;
    Score: TText;
    BestScore: TText;
    PaintFree: TPaintBox;
    ImageSettings: TImage;
    VidiLogo: TImage;
    VidiBanner: TImage;
    GameOver: TLayout;
    ImageGameover: TImage;
    GameOverText: TText;
    ImageCrown: TImage;
    TimerGameOver: TTimer;
    MediaPlayer1: TMediaPlayer;
    AnimateTimer: TTimer;
    TopRectangle:TRectangle;
    procedure MainPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MainResize(Sender: TObject);
    procedure MainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure MainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure MainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure PaintFreePaint(Sender: TObject; Canvas: TCanvas);
    procedure ImageSettingsClick(Sender: TObject);
    procedure GameOverClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure GameOverTextResize(Sender: TObject);
    procedure VidiBannerClick(Sender: TObject);
    procedure TimerGameOverTimer(Sender: TObject);
    procedure VidiLogoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImageCrownClick(Sender: TObject);
  private
    { Private declarations }

    FClose : Boolean;

    VidiCanvas : TVidiCanvas;

    Blocks : TBlocks;

    function ActiveMatch:Boolean;
    procedure AnimateLogo;

    procedure BlocksInvalidate(Sender: TObject);
    procedure BlocksPlaySound(Sender:TObject; const AName:String);
    procedure BlocksRefreshScore(Sender: TObject);
    procedure BlocksSetCursor(Sender:TObject; const ACursor:TMouseCursor);
    procedure BlocksTerminate(Sender: TObject);

    procedure ChangeGameOverText;
    procedure EnableTimer(const Value:Boolean);
    function GetTick:Cardinal; inline;

    procedure GameOverAdjustFontSize;
    procedure LoadMatch;
    procedure LoadSettings;
    procedure RefreshScore;
    procedure ResizeBestScore;
    procedure SaveMatch;
    procedure SaveSettings;
    procedure TrySaveBest;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

procedure Debug(const AMessage:String);

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}

  System.IOUtils, FMX.DialogService, IniFiles,

  Unit_Settings, Unit_Texts, Unit_Sounds, Unit_Animate, Vidi_FMX,
  Unit_BestScores;

{$R *.fmx}

{$IFDEF MSWINDOWS}
{$R 'vidi_sounds.res' '..\Sounds\vidi_sounds.rc'}
{$ENDIF}

function GetDocumentFile(const AName:String):String;
begin
  result:=TPath.Combine(TPath.GetDocumentsPath,'VidiBlocks');
  result:=TPath.Combine(result,AName);
end;

function ScoreFile:String;
begin
  result:=GetDocumentFile(Vidi_Text_ScoreFile);
end;

function MatchFile:String;
begin
  result:=GetDocumentFile(Vidi_Text_MatchFile);
end;

function BestScoresFile:String;
begin
  result:=GetDocumentFile(Vidi_Text_BestScoresFile);
end;

function SettingsFile:String;
begin
  result:=GetDocumentFile(Vidi_Text_SettingsFile);
end;

const
  General='General';

procedure TFormMain.LoadMatch;
var S : TStrings;
    N : Integer;
begin
  S:=TStringList.Create;
  try
    S.LoadFromFile(MatchFile);

    N:=0;

    Blocks.LoadMatch(function:Integer
    begin
      result:=StrToInt(S[N]);
      Inc(N);
    end);

  finally
    S.Free;
  end;
end;

procedure TFormMain.LoadSettings;
var S : String;
begin
  S:=SettingsFile;

  if TFile.Exists(S) then
  with TIniFile.Create(S) do
  try
    Blocks.Silent:=ReadBool(General,'Silent',False);
    Blocks.LockRotation:=ReadBool(General,'LockRotation',False);

    Blocks.Language:=ReadString(General,'Language','');

    if Blocks.Language='' then
       SetLanguage(Vidi_DefaultLanguage)
    else
       SetLanguage(Blocks.Language);
  finally
    Free;
  end;
end;

procedure TFormMain.SaveSettings;
var S : String;
begin
  S:=SettingsFile;

  ForceDirectories(ExtractFilePath(S));

  with TIniFile.Create(S) do
  try
    WriteBool(General,'Silent',Blocks.Silent);
    WriteBool(General,'LockRotation',Blocks.LockRotation);

    WriteString(General,'Language',Blocks.Language);
  finally
    Free;
  end;
end;

procedure Debug(const AMessage:String);
begin
  TDialogService.MessageDialog(AMessage,TMsgDlgType.mtWarning,
    [TMsgDlgBtn.mbOK],
    TMsgDlgBtn.mbOK,0,
          procedure(const AResult: TModalResult)
          begin
          end);
end;

procedure SureToLeave(const ACloseProc:TInputCloseDialogProc);
begin
  TDialogService.MessageDialog(Vidi_Text_SureToStop,TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbNo,0,ACloseProc);
end;

procedure TFormMain.ChangeGameOverText;
begin
  GameOverText.Text:=Vidi_Text_GameOver;

  GameOverAdjustFontSize;
end;

function TFormMain.ActiveMatch:Boolean;
begin
  result:=(Blocks.Score>0) and (not GameOver.Visible);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not ActiveMatch then
     CanClose:=True
  else
  begin
    if not FClose then
    begin
      Blocks.PlaySound('click');

      SureToLeave(
          procedure(const AResult: TModalResult)
          begin
            if AResult=mrYes then
            begin
              FClose:=True;
              Close;
            end;
          end);
    end;

    CanClose:=FClose;
  end;
end;

procedure TFormMain.EnableTimer(const Value:Boolean);
begin
  AnimateTimer.Enabled:=Value;
end;

function TFormMain.GetTick:Cardinal;
begin
  result:=TThread.GetTickCount;
end;

procedure TFormMain.FormCreate(Sender: TObject);

  procedure CreateBlocks;
  begin
    VidiCanvas:=TVidiCanvas.Create;
    VidiCanvas.Canvas:=Main.Canvas;

    Blocks:=TBlocks.Create(VidiCanvas);

    Blocks.Invalidate:=BlocksInvalidate;
    Blocks.RefreshScore:=BlocksRefreshScore;
    Blocks.SetCursor:=BlocksSetCursor;
    Blocks.Terminate:=BlocksTerminate;
    Blocks.DoPlaySound:=BlocksPlaySound;

    Blocks.Resize(TRectFloat(Main.BoundsRect));
  end;

  procedure SetupAnimate;
  begin
    AnimateTimer.Interval:=16; // 1000/60 frames per second
    AnimateTimer.OnTimer:=TAnimate.TimerEvent;

    TAnimate.OnEnable:=EnableTimer;
    TAnimate.GetTick:=GetTick;
  end;

  procedure LoadBestScores;
  var F : TFileStream;
      R : TReader;
      t, N : Integer;
  begin
    F:=TFileStream.Create(BestScoresFile,fmOpenRead);
    try
      R:=TReader.Create(F,1024);
      try
        N:=R.ReadInteger;
        SetLength(Blocks.BestScores,N);

        for t:=0 to N-1 do
        begin
          Blocks.BestScores[t].Score:=R.ReadInteger;
          Blocks.BestScores[t].Moves:=R.ReadInteger;
          Blocks.BestScores[t].Date:=R.ReadDate;
        end;
      finally
        R.Free;
      end;
    finally
      F.Free;
    end;
  end;

begin
  SetupAnimate;

  {$IFNDEF MSWINDOWS}
  Sound_MediaPlayer:=MediaPlayer1;
  {$ENDIF}

  Main.AutoCapture:=True;
  Randomize;

  GameOver.Visible:=False;

  CreateBlocks;

  LoadSettings;

  if TFile.Exists(BestScoresFile) then
     LoadBestScores;
end;

procedure TFormMain.BlocksInvalidate(Sender: TObject);
begin
  Main.Repaint;
end;

procedure TFormMain.ResizeBestScore;
begin
  BestScore.AutoSize:=False;
  BestScore.AutoSize:=True;
  BestScore.TextSettings.VertAlign:=TTextAlign.Center;
end;

procedure TFormMain.RefreshScore;
begin
  Score.Text:=IntToStr(Blocks.Score);
end;

procedure TFormMain.BlocksRefreshScore(Sender: TObject);
begin
  RefreshScore;

  BestScore.Text:=IntToStr(Blocks.BestScore);

  if Blocks.Score=Blocks.BestScore then
     BestScore.TextSettings.FontColor:=TAlphaColorRec.Orangered
  else
     BestScore.TextSettings.FontColor:=TAlphaColorRec.Brown;

  ResizeBestScore;

  PaintFree.Repaint;
end;

procedure TFormMain.BlocksSetCursor(Sender:TObject; const ACursor:TMouseCursor);
begin
  case ACursor of
   TMouseCursor.Normal: Main.Cursor:=crDefault;
   TMouseCursor.Disabled: Main.Cursor:=crNoDrop;
  else
     Main.Cursor:=crHandPoint;
  end;
end;

procedure TFormMain.BlocksTerminate(Sender: TObject);

  procedure AddBestScore;
  var tmp : TScoreItem;
  begin
    tmp.Date:=Now;
    tmp.Score:=Blocks.BestScore;
    tmp.Moves:=Blocks.Moves;

    Insert(tmp,Blocks.BestScores,0);
  end;

  procedure SaveBestScores;
  var F : TFileStream;
      B : TScoreItem;
      W : TWriter;
  begin
    F:=TFileStream.Create(BestScoresFile,fmCreate);
    try
      W:=TWriter.Create(F,1024);
      try
        W.WriteInteger(Length(Blocks.BestScores));

        for B in Blocks.BestScores do
        begin
          W.WriteInteger(B.Score);
          W.WriteInteger(B.Moves);
          W.WriteDate(B.Date);
        end;
      finally
        W.Free;
      end;
    finally
      F.Free;
    end;
  end;

begin
  if Blocks.Score=Blocks.BestScore then
  begin
    AddBestScore;
    SaveBestScores;
  end;

  ChangeGameOverText;
  GameOver.Visible:=True;

  TAnimate.Linear(1000,procedure(const Value:Single)
  begin
    {if Value=1 then
       GameOverText.Scale.X:=1
    else}
    GameOverText.Opacity:=Value;
    GameOverText.Scale.X:=0.5+Value*0.5;
  end);

  TimerGameOver.Enabled:=True;
end;

procedure TFormMain.BlocksPlaySound(Sender:TObject; const AName:String);
begin
  {$IFDEF MSWINDOWS}
  Vidi_PlaySound_Resource('sound_'+AName);
  {$ELSE}
  Vidi_PlaySound(AName);
  {$ENDIF}
end;

procedure TFormMain.TimerGameOverTimer(Sender: TObject);
begin
  TimerGameOver.Enabled:=False;
  GameOverText.Text:=Vidi_Text_PlayAgain;
end;

procedure TFormMain.TrySaveBest;
var S : String;
begin
  if Blocks.BestScore>Blocks.OldBest then
  begin
    S:=ScoreFile;

    ForceDirectories(ExtractFilePath(S));

    if TFile.Exists(S) then
       TFile.Delete(S);

    TFile.WriteAllText(S,IntToStr(Blocks.BestScore));
    Blocks.OldBest:=Blocks.BestScore;
  end;
end;

procedure TFormMain.VidiLogoClick(Sender: TObject);
begin
  TAnimate.Linear(500,
    procedure(const Value:Single)
    begin
      if Value<0.5 then
         VidiBanner.Scale.X:=1-Value
      else
         VidiBanner.Scale.X:=Value;
    end);
end;

procedure TFormMain.SaveMatch;
var S : TStrings;
    Match : String;
begin
  S:=TStringList.Create;
  try
    Match:=MatchFile;
    ForceDirectories(ExtractFilePath(Match));

    Blocks.SaveMatch(procedure(const Value:Integer)
    begin
      S.Add(IntToStr(Value));
    end);

    S.SaveToFile(Match);
  finally
    S.Free;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if ActiveMatch then
     SaveMatch;

  TrySaveBest;
  Blocks.Free;

  //VidiCanvas.FCanvas:=nil;
  //VidiCanvas.Free;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key=vkF11 then
     FullScreen:=not FullScreen;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if Width>600 then
  begin
    if TopRectangle.Parent=Self then
    begin
      TopRectangle.Parent:=TopLayout;
      TopRectangle.Align:=TAlignLayout.MostRight;
      TopRectangle.Width:=396;

      ResizeBestScore;
    end;
  end
  else
  if TopRectangle.Parent=TopLayout then
  begin
    TopRectangle.Parent:=Self;
    TopRectangle.Align:=TAlignLayout.MostTop;
    TopRectangle.Height:=65;

    ResizeBestScore;
  end;
end;

procedure TFormMain.AnimateLogo;
begin
  TAnimate.Linear(750,
    procedure(const Value:Single)
    begin
      if Value=1 then
         VidiLogo.RotationAngle:=0
      else
         VidiLogo.RotationAngle:=360*Value;
    end);
end;

procedure TFormMain.FormShow(Sender: TObject);

  procedure LoadScores;
  var S : String;
  begin
    S:=ScoreFile;

    if TFile.Exists(S) then
    begin
      Blocks.BestScore:=StrToInt(TFile.ReadAllText(S));
      Blocks.OldBest:=Blocks.BestScore;
    end;
  end;

  procedure TryLoadMatch;
  begin
    if TFile.Exists(MatchFile) then
    begin
      LoadMatch;
      TFile.Delete(MatchFile);

      Blocks.CalculateDisabled;

      RefreshScore;
    end;
  end;

begin
  Blocks.PlaySound('aire_boomerang');
  LoadScores;
  BlocksRefreshScore(Self);

  AnimateLogo;

  TryLoadMatch;
end;

procedure TFormMain.GameOverClick(Sender: TObject);
begin
  GameOver.Visible:=False;

  TrySaveBest;
  Blocks.NewGame;
  BlocksRefreshScore(Self);
end;

procedure TFormMain.GameOverAdjustFontSize;
begin
  if GameOver.Visible then
     AdjustFontSize(GameOverText,48);
end;

procedure TFormMain.GameOverTextResize(Sender: TObject);
begin
  GameOverAdjustFontSize;
end;

procedure TFormMain.VidiBannerClick(Sender: TObject);
begin
  AnimateLogo;
end;

procedure TFormMain.ImageCrownClick(Sender: TObject);
begin
  TFormScores.Modal(Self,Blocks.BestScores,
  procedure (Value:TModalResult)
  begin
  end);
end;

procedure TFormMain.ImageSettingsClick(Sender: TObject);
begin
  Blocks.PlaySound('click');

  TFormSettings.Modal(Self,Blocks,
    procedure(AResult: TModalResult)
    begin
      ChangeGameOverText;

      SaveSettings;
      Invalidate;
    end);
end;

procedure TFormMain.MainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not GameOver.Visible then
     Blocks.MouseDown(X,Y);
end;

procedure TFormMain.MainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  Blocks.MouseMove(X,Y);
end;

procedure TFormMain.MainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Blocks.MouseUp(X,Y);
end;

{.$DEFINE MONITOR}
{$IFDEF MONITOR}
var
  Monitor : Integer;

procedure PaintMonitor(const ACanvas:TCanvas; const AValue:Integer);
begin
  ACanvas.FillText(TRectF.Create(4,4,30,20),
      IntToStr(AValue),False,1,[],TTextAlign.Leading);
end;
{$ENDIF}

procedure TFormMain.MainPaint(Sender: TObject; Canvas: TCanvas);
begin
  VidiCanvas.Canvas:=Canvas;
  Blocks.Paint;

  {$IFDEF MONITOR}
  Inc(Monitor);

  PaintMonitor(Canvas,Blocks.BigLimit {Monitor});
  {$ENDIF}
end;

procedure TFormMain.MainResize(Sender: TObject);
begin
  if Blocks<>nil then
     Blocks.Resize(TRectFloat(Main.BoundsRect));
end;

procedure TFormMain.PaintFreePaint(Sender: TObject; Canvas: TCanvas);
var APercentage : Single;
    R : TRectF;
begin
  PaintFree.Canvas.Fill.Kind:=FMX.Graphics.TBrushKind.Solid;
  PaintFree.Canvas.Fill.Color:=TAlphaColorRec.White;
  PaintFree.Canvas.FillRect(PaintFree.LocalRect,1);

  PaintFree.Canvas.Fill.Color:=Blocks.PaintFree(APercentage);

  R:=PaintFree.LocalRect;
  R.Top:=R.Top+(R.Height-(APercentage*R.Height));
  PaintFree.Canvas.FillRect(R,1);
end;

{$IFDEF MONITOR}
initialization
  Monitor:=0;
{$ENDIF}
end.
