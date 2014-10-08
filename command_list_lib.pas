unit command_list_lib;

interface

uses classes,abstract_command_lib;

type

TCommandList=class(TAbstractCommandContainer) //список для undo/redo и даже для сохранения данных в файл
  private
    fRoot: TComponent;
    fcount: Integer;
    fcurrent: Integer; //наше данное положение - куда добавлять команду. Т.е по умолчанию - 0

    procedure ReadCount(reader: TReader); //основательная инкапсуляция- держим сведения
    procedure WriteCount(writer: TWriter);  //недоступными для изм. из программы,
    procedure ReadCurrent(reader: TReader); //но из файла считать можем
    procedure WriteCurrent(writer: TWriter);
  protected
    procedure DefineProperties(filer: TFiler); override;
  public
    constructor Create(Aowner: TComponent); override;

    procedure Add(command: TAbstractCommand); override;
    procedure Undo; override;
    procedure Redo; override;
    function UndoEnabled: Boolean; override;
    function RedoEnabled: Boolean; override;
    destructor Destroy; override;
    procedure Clear; override;
    property count: Integer read fcount;
    property current: Integer read fcurrent;
  end;


implementation
(*
            TCommandList
                                  *)

constructor TCommandList.Create(AOwner: TComponent);
var tmp: TComponent;
begin
  inherited Create(AOwner);
  if Aowner=nil then FRoot:=self
  else begin
    tmp:=Aowner;
    repeat
      FRoot:=tmp;
      tmp:=tmp.Owner;
    until tmp=nil;
  end;
end;

procedure TCommandList.Add(command: TAbstractCommand);
var i:Integer;
begin
  for i:=fcount-1 downto fcurrent do components[i].Free;
  insertComponent(command);
  inc(fcurrent);
  fcount:=fcurrent;
end;

procedure TCommandList.Undo;
var res: Boolean;
begin
  if UndoEnabled then begin
    dec(fcurrent);
    res:=(components[fcurrent] as TAbstractCommand).Undo;
    Assert(res,'undo command failed');
  end;
end;

procedure TCommandList.Redo;
var res: Boolean;
begin
  if RedoEnabled then begin
    res:=(components[fcurrent] as TAbstractCommand).Execute;
    Assert(res,'redo command failed');
    inc(fcurrent);
  end;
end;

function TCommandList.UndoEnabled: boolean;
begin
  Result:=(fcurrent>0);
end;

function TCommandList.RedoEnabled: boolean;
begin
  Result:=(fcurrent<fcount);
end;

destructor TCommandList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCommandList.Clear;
begin
  self.DestroyComponents;
  fcurrent:=0;
  fcount:=0;
end;

procedure TCommandList.DefineProperties(filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('count',ReadCount,WriteCount,(fcount>0));
  Filer.DefineProperty('current',ReadCurrent,WriteCurrent,(fcurrent>0));
end;

procedure TCommandList.ReadCount(reader: TReader);
begin
  fcount:=reader.ReadInteger;
end;

procedure TCommandList.WriteCount(writer: TWriter);
begin
  writer.writeInteger(fcount);
end;

procedure TCommandList.ReadCurrent(reader: TReader);
begin
  fcurrent:=reader.ReadInteger;
end;

procedure TCommandList.WriteCurrent(writer: TWriter);
begin
  writer.writeInteger(fcurrent);
end;

initialization
  RegisterClasses([TCommandList]);

end.
