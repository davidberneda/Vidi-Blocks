// @davidberneda Jan 2024
program VidiBlocks;

uses
  System.StartUpCopy,
  FMX.Forms,

  //FMX.Skia,

  Unit_Blocks in 'fmx\Unit_Blocks.pas' {FormMain},
  Unit_Settings in 'fmx\Unit_Settings.pas' {FormSettings},
  Unit_Texts in 'fmx\Unit_Texts.pas',
  Vidi_FMX in 'fmx\Vidi_FMX.pas',
  Unit_Animate in 'common\Unit_Animate.pas',
  Unit_Game in 'common\Unit_Game.pas',
  Vidi_Graphics in 'common\Vidi_Graphics.pas',
  Vidi_Game in 'fmx\Vidi_Game.pas',
  Vidi_Canvas in 'common\Vidi_Canvas.pas',
  Unit_Sounds in 'common\Unit_Sounds.pas',
  Unit_BestScores in 'fmx\Unit_BestScores.pas' {FormScores};

{$R *.res}

begin
  //GlobalUseSkia := False;

  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
