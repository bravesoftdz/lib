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

  end;

implementation

uses SysUtils,StrUtils;

function GoodOlCompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if List.CaseSensitive then
    Result := AnsiCompareStr(List[Index1], List[Index2])
  else
    Result := AnsiCompareText(List[Index1], List[Index2]);
end;

function TAutocompleteStringList.CompareStrings(const S1,S2: string): Integer;
begin
  Result:=inherited CompareStrings(LeftStr(S1,Length(S2)),S2);
end;

procedure TAutocompleteStringList.Sort;
begin
  CustomSort(GoodOlCompareStrings);
end;

end.
