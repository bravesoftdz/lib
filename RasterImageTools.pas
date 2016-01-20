unit RasterImageTools;

interface

uses types,classes,controls,command_class_lib,image_on_demand,RasterImageCommands;

type

TRasterImageTool = class (TAbstractToolAction)
  protected
    function doc: TRasterImageDocument;
end;

TBrushTool = class (TRasterImageTool)
  private
    fBrushing: Boolean;
    fShowBrush: Boolean;
    fBrushCommand: TBrushCommand;
    fBrushPosX,fBrushPosY: Integer;
    procedure DrawBrush;
  public
    function Select: Boolean; override;
    procedure Unselect; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseWheelUp(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); override;
    procedure MouseWheelDown(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); override;
end;

Procedure Register;

implementation

uses graphics,ActnList;

resourcestring
  BrushToolCaption='Кисть';

procedure Register;
begin
  RegisterActions('RasterImageActions',[TBrushTool],nil);
end;

(*
    TRasterImageTool
                        *)
function TRasterImageTool.doc: TRasterImageDocument;
begin
  Result:=owner as TRasterImageDocument;
end;

(*
    TBrushTool
                  *)
function TBrushTool.Select: Boolean;
begin
  SetStatusPanel(BrushToolCaption);
  Result:=true;
end;

procedure TBrushTool.Unselect;
begin
  SetStatusPanel('');
end;

procedure TBrushTool.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fBrushing:=true;
  fShowBrush:=false;
  fBrushCommand:=TBrushCommand.Create(nil);
  MouseMove(Shift,X,Y);
end;

procedure TBrushTool.MouseMove(Shift: TShiftState; X,Y: Integer);
var BrushRect: TRect;
begin
  if fBrushing then begin
    fBrushCommand.Draw(Round(X/doc.get_Scale),Round(Y/doc.Get_Scale));
    BrushRect:=Rect(X-doc.BrushSize div 2,Y-doc.BrushSize div 2,
                (X+(doc.BrushSize-1) div 2)+1, (Y+(doc.BrushSize-1) div 2)+1);
    with doc.Image.Canvas do begin
      Brush.Color:=doc.PrimaryColor;
      Brush.Style:=bsSolid;
      Pen.Width:=0;
      if doc.BrushShape=bsSquare then
        FillRect(BrushRect)
      else if doc.BrushShape=bsRound then
        Ellipse(BrushRect);
    end;
  end
  else begin
    if fShowBrush then
      DrawBrush;
    fShowBrush:=true;
    fBrushPosX:=X;
    fBrushPosY:=Y;
    DrawBrush;
  end;
end;

procedure TBrushTool.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fBrushing:=false;
  //сейчас рамочку сотрут, и мы должны про это сказать
  fShowBrush:=false;
  if not doc.DispatchCommand(fBrushCommand) then
    fShowBrush:=true;
end;

procedure TBrushTool.DrawBrush;
var BrushSize: Integer;
begin
  BrushSize:=doc.BrushSize;
  with doc.Image.Picture.Bitmap.Canvas do begin
    Pen.Mode:=pmNotXor;
    Pen.Width:=1;
//    Pen.Mode:=pmBlack;
//    Pen.Mode:=pmNot;
    Pen.Color:=clBlack;
//    Brush.Color:=clWhite;
//    Brush.Style:=bsClear;
    Pen.Width:=1;
    Rectangle(Rect(fBrushPosX-BrushSize,fBrushPosY-BrushSize,
      fBrushPosX+BrushSize,fBrushPosY+BrushSize));
  end;
end;

procedure TBrushTool.MouseWheelDown(Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if doc.BrushSize>0 then begin
    DrawBrush;
    doc.BrushSize:=doc.BrushSize-1;
    DrawBrush;
  end;
  Handled:=true;
end;

procedure TBrushTool.MouseWheelUp(Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  DrawBrush;
  doc.BrushSize:=doc.BrushSize+1;
  DrawBrush;
  Handled:=true;
end;


initialization
  RegisterClasses([TBrushTool]);
end.
