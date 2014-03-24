unit streamable_component_list;

interface

uses classes,streaming_class_lib;

type

TComponentClass=class of TComponent;

TStreamableComponentList=class(TStreamingClass)
  private
    fResolved: boolean;
    fList: TStrings;
    procedure ResolveNames;
    procedure SetList(writer: TWriter);
    procedure GetList(reader: TReader);
    function GetItem(index: Integer): TCOmponent;
    function GetCount: Integer;
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Add(component: TComponent);
    property Count: Integer read GetCount;
    property Item[index: Integer]: TComponent read GetItem; default;
end;

implementation

constructor TStreamableComponentList.Create(owner: TComponent);
begin
  inherited Create(owner);
  fList:=TStringList.Create;
  fResolved:=true;
end;

destructor TStreamableComponentList.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TStreamableComponentList.Add(component: TComponent);
begin
  fList.AddObject(component.Name,component);
end;

procedure TStreamableComponentList.ResolveNames;
var i: Integer;
    c: TComponent;
begin
  for i:=0 to fList.Count-1 do begin
    fList.Objects[i]:=FindNestedComponent(FindOwner,fList.Strings[i]);
  end;
  fResolved:=true;
end;

procedure TStreamableComponentList.Loaded;
begin
  if not fResolved then ResolveNames;
end;

procedure TStreamableComponentList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('data',GetList,SetList,fList.Count>0);
end;

procedure TStreamableComponentList.SetList(writer: TWriter);
var i: Integer;
begin
  writer.WriteListBegin;
  for i:=0 to fList.Count-1 do begin
    writer.WriteIdent(fList.Strings[i]);
  end;
  writer.WriteListEnd;
end;

procedure TStreamableComponentList.GetList(reader: TReader);
begin
  reader.ReadListBegin;
    while not reader.EndOfList do begin
      fList.Add(reader.ReadIdent);
    end;
  reader.ReadListEnd;
  fResolved:=false;
end;

function TStreamableComponentList.GetItem(index: Integer): TCOmponent;
begin
  if not fResolved then ResolveNames;
  Result:=fList.Objects[index] as TComponent;
end;

function TStreamableComponentList.GetCount: Integer;
begin
  Result:=fList.Count;
end;

end.
