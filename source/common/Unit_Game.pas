unit Unit_Game;

interface

uses
  Vidi_Graphics;

type
  TInvalidateEvent=procedure(Sender:TObject) of object;
  TTerminateEvent=procedure(Sender:TObject) of object;
  TPlaySoundEvent=procedure(Sender:TObject; const Name:String) of object;

  TMouseCursor=(Normal,Hand,Disabled);
  TSetCursorEvent=procedure(Sender:TObject; const ACursor:TMouseCursor) of object;

  TRefreshScoreEvent=procedure(Sender:TObject) of object;

  TIntegerFunction=reference to function:Integer;
  TIntegerProcedure=reference to procedure(const Value:Integer);

  TScoreItem=record
  public
    Date : TDateTime;
    Moves : Integer;
    Score : Integer;
  end;

  TBestScores=TArray<TScoreItem>;

  TGame=class
  protected
  public
    Canvas : ICanvas;

    Silent : Boolean;

    Invalidate : TInvalidateEvent;
    DoPlaySound : TPlaySoundEvent;
    SetCursor : TSetCursorEvent;
    Terminate : TTerminateEvent;
    RefreshScore : TRefreshScoreEvent;

    Width,
    Height : Single;

    Bounds : TRectFloat;

    VerticalMode : Boolean;

    Score,
    OldBest,
    BestScore : Integer;

    BestScores : TBestScores;

    Constructor Create(const ACanvas:ICanvas); virtual;

    procedure PlaySound(const AName:String);
    procedure Resize(const ABounds:TRectFloat); virtual;
  end;

implementation

{ TGame }

Constructor TGame.Create(const ACanvas: ICanvas);
begin
  inherited Create;
  Canvas:=ACanvas;
end;

procedure TGame.Resize(const ABounds:TRectFloat);
begin
  Bounds:=ABounds;

  Width:=ABounds.Width;
  Height:=ABounds.Height;

  VerticalMode:=Height>Width;
end;

procedure TGame.PlaySound(const AName:String);
begin
  if not Silent then
     DoPlaySound(Self,AName);
end;

end.
