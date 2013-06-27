unit iterator_lib;

interface

uses classes;

type

TIterator=class(TComponent)
  private
    fclassname: TClass;
    function get_component(i: Integer): TComponent;
  public
    constructor Create(owner: TComponent;classname: TClass); reintroduce; overload;
    function count: Integer;
    property Component[i: Integer]: TComponent read get_component; default;
end;

implementation

constructor TIterator.Create(owner: TComponent;classname: TClass);
begin
  inherited Create(owner);
  fclassname:=classname;
  SetSubComponent(true);
end;

function TIterator.count: Integer;
var i,j: Integer;
begin
  j:=0;
  for i:=0 to owner.ComponentCount-1 do begin
    if owner.Components[i] is fclassname then inc(j);
  end;
  result:=j;
end;

function TIterator.get_component(i: Integer): TComponent;
var j,k: Integer;
//  comp: TComponent;
begin
  j:=-1;
  k:=-1;
  while k<i do begin
    inc(j);
    if owner.Components[j] is fclassname then inc(k);
  end;
  Result:=owner.Components[j];
end;

end.
