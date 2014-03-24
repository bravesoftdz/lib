unit streamable_component_list;

interface

uses classes,streaming_class_lib;

type

TComponentClass=class of TComponent;

TStreamableComponentList=class(TStreamingClass)
  private
    fResolved: boolean;
    fList: TList;
    procedure LoadList(reader: TReader);
    procedure WriteList(writer: TWriter);
    procedure ResolveNames;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Add(component: TComponent);
end;

implementation

constructor TStreamableComponentList.Create(owner: TComponent);
begin
  inherited Create(owner);
  fList:=TList.Create;
  fResolved:=true;
end;

destructor TStreamableComponentList.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TStreamableComponentList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('data',LoadList,WriteList,fList.Count>0);
end;

procedure TStreamableComponentList.WriteList(writer: TWriter);
var i: Integer;
begin
  if not fResolved then ResolveNames;
  writer.WriteListBegin;
    for i:=0 to fList.Count-1 do begin
      writer.WriteIdent(TComponent(fList.Items[i]).name);
    end;
  writer.WriteListEnd;
end;

procedure TStreamableComponentList.LoadList(reader: TReader);
var s: string;
begin
  reader.ReadListBegin;
    while not reader.EndOfList do begin
      s:=reader.ReadIdent;
      fList.Add(Pointer(s));
      Pointer(s):=nil;
    end;
  reader.ReadListEnd;
  fResolved:=false;
end;

procedure TStreamableComponentList.Add(component: TComponent);
begin
  fList.Add(component);
end;

procedure TStreamableComponentList.ResolveNames;
var i: Integer;
  s: string;
begin
//нужно срочно превратить ссылки на строки в ссылки на объекты
  for i:=0 to fList.Count-1 do begin
    s:=string(fList.Items[i]);
    if s='' then fList.Items[i]:=nil;
  end;
end;

procedure TStreamableComponentList.Loaded;
begin
  if not fResolved then ResolveNames;
end;

end.
