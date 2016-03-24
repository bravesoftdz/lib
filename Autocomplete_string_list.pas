unit Autocomplete_string_list;

interface

uses classes;

type

  TAutocompleteStringList = class (TStringList)
    protected
      function CompareStrings(const S1, S2: string): Integer; override;
      //ищет не точное соответствие, а лишь то, что S1 начинается с S2
      //сортировка, к сожалению, работает через нее же
      //могут быть проблемы
    public
      procedure Sort; override;
      function GetListOfObjects(str: string):TStringList;
  end;

implementation

uses SysUtils,StrUtils,localized_string_lib;

function GoodOlCompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if List.CaseSensitive then
    Result:=LocaleCompareStr(List[Index1],List[Index2])
  else
    Result := LocaleCompareText(List[Index1], List[Index2]);
end;

function TAutocompleteStringList.CompareStrings(const S1,S2: string): Integer;
begin
  Result:=inherited CompareStrings(LeftStr(S1,Length(S2)),S2);
end;

procedure TAutocompleteStringList.Sort;
begin
  CustomSort(GoodOlCompareStrings);
end;

function TAutocompleteStringList.GetListOfObjects(str: string): TStringList;
var i,u,d: Integer;
begin
  Result:=TStringList.Create;
  if Find(str,d) then begin
    //Find гарантированно выдаст первое упоминание str
    u:=d+1;
    while (u<Count) and (CompareStrings(strings[u],str)=0) do inc(u);
    dec(u);
    for i:=d to u do
      Result.AddObject(Strings[i],Objects[i]);
  end;
end;

end.
