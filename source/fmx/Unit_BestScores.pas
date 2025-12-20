unit Unit_BestScores;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Objects, FMX.Layouts,

  Unit_Game;

type
  TFormScores = class(TForm)
    Rectangle1: TRectangle;
    Image2: TImage;
    Image3: TImage;
    Layout1: TLayout;
    Text1: TText;
    ListScores: TListView;
    procedure Image2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    Scores : TBestScores;
  public
    { Public declarations }

    class procedure Modal(const AOwner:TComponent;
       const AScores:TBestScores;
       const AResult:TProc<TModalResult>); static;
  end;

var
  FormScores: TFormScores;

implementation

{$R *.fmx}

procedure TFormScores.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=TCloseAction.caFree;
end;

procedure TFormScores.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=vkEscape then
     Close;
end;

procedure TFormScores.FormShow(Sender: TObject);
var S : TScoreItem;
begin
  ListScores.BeginUpdate;
  try
    ListScores.Items.Clear;

    for S in Scores do
    with ListScores.Items.Add do
    begin
      Text:=IntToStr(S.Score);
      Detail:=DateTimeToStr(S.Date)+' '+IntToStr(S.Moves)+' moves';
    end;
  finally
    ListScores.EndUpdate;
  end;
end;

procedure TFormScores.Image2Click(Sender: TObject);
begin
  Close;
end;

class procedure TFormScores.Modal(const AOwner: TComponent;
  const AScores:TBestScores; const AResult: TProc<TModalResult>);
var tmp : TFormScores;
begin
  tmp:=TFormScores.Create(AOwner);
  tmp.Scores:=AScores;

  tmp.ShowModal(AResult);
end;

end.
