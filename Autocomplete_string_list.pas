unit Autocomplete_string_list;

interface

uses classes;

type

  TAutocompleteStringList = class (TStringList)
    protected
      function CompareStrings(const S1, S2: string): Integer; override;
      //���� �� ������ ������������, � ���� ��, ��� S1 ���������� � S2
      //����������, � ���������, �������� ����� ��� ��
      //����� ���� ��������
    public
      procedure Sort; override;
      function GetListOfObjects(str: string):TStringList;
  end;

implementation

uses SysUtils,StrUtils,set_english_locale_if_not_sure;

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
  if Find(str,i) then begin
    //����� �������� � �����, � ����!
    d:=i-1;
    while (d>-1) and (CompareStrings(strings[d],str)=0) do dec(d);
    inc(d);
    u:=i+1;
    while (u<Count) and (CompareStrings(strings[u],str)=0) do inc(u);
    dec(u);
    for i:=d to u do
      Result.AddObject(Strings[i],Objects[i]);
  end;
end;

end.
