unit EggActivityDiagram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  fgl,
  EggCustomControl;

type
  TEggActivityList = class;

  { TEggActivityItem }

  TEggActivityItem = class(TObject)
  private
    FActivityClass: String;
    FCaption: String;
    FComment: String;
    FEnabled: Boolean;

    procedure SetActivityClass(AValue: String);
    procedure SetCaption(AValue: String);
    procedure SetComment(AValue: String);
    procedure SetEnabled(AValue: Boolean);
  public
    property ActivityClass: String read FActivityClass write SetActivityClass;
    property Caption: String read FCaption write SetCaption;
    property Comment: String read FComment write SetComment;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TEggActivitys = specialize TFPGObjectList<TEggActivityItem>;

  TEggActivityList = class(TEggActivitys)
  public
  end;

  TEggActivityDiagram = class(TEggCustomControl)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I EggActivityDiagram_icon.lrs}
  RegisterComponents('Egg',[TEggActivityDiagram]);
end;

{ TEggActivityItem }

procedure TEggActivityItem.SetActivityClass(AValue: String);
begin
  if FActivityClass = AValue then Exit;
  FActivityClass := AValue;
end;

procedure TEggActivityItem.SetCaption(AValue: String);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
end;

procedure TEggActivityItem.SetComment(AValue: String);
begin
  if FComment = AValue then Exit;
  FComment := AValue;
end;

procedure TEggActivityItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
end;

end.
