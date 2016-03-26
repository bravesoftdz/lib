unit StreamingFormatCyr;

interface

const sfCyr = 2;

implementation

uses classes, sysUtils, streaming_class_lib;

procedure ObjectTextToCyr(input,output: TStream);
var c: Char;
    i: Integer;
    inside_string: Integer;
    apostr: Char;
    getback: string;
begin
  apostr:='''';
  input.Seek(0,soFromBeginning);
  inside_string:=0; //��� ����� �� ������� ������
  while input.Position<input.Size do begin
    input.Read(c,1);
    if inside_string=1 then begin
      if c=apostr then begin
        inside_string:=2;  //����� �� ������, �� �������� ������� �� ������
//        continue;
      end
      else output.write(c,1); //��������� ��� ������� ������ ������
    end
    else begin
      //�� �������, ���� inside_String=0 ������ ��� � ����,
      //���� =2, ������ �� ���� �������.
      if c=apostr then begin
        if inside_string=0 then output.Write(apostr,1);
        inside_string:=1; //� ���� �� ��� ��� 2, ������ �� ��� � �� ����� �� ������, ������� �� �����
      end
      else if (c<>'#') then begin
        if inside_string=2 then begin
          inside_string:=0;
          output.Write(apostr,1);
        end;
        output.Write(c,1);
        end
      else begin
        //����� �������� - �� ������� #
        input.Read(c,1);
        if not (c in ['0'..'9']) then begin
          if inside_string=2 then begin
            inside_string:=0;
            getback:=apostr+'#';
          end
          else getback:='#';
          output.Write(getback[1],Length(getback));
          input.Seek(-1,soFromCurrent);
        end
        else begin
          i:=0;
          while c in ['0'..'9'] do begin
            i:=i*10+Ord(c)-Ord('0');
            input.Read(c,1);
          end;
          input.Seek(-1,soFromCurrent);
          //������ ��� ��������� ���� ������������ � ���������

          if i<255 then begin
            if inside_string=2 then begin
              inside_string:=0;
              output.Write(apostr,1); //���-���� ����� ������
              //������� ��������, �������� ��������� �����
            end;
            getback:='#'+IntToStr(i);
            output.Write(getback[1],Length(getback));
          end
          else begin
            c:=CHR(Byte(i+Ord('�')-1040));
            if inside_string=0 then begin
              inside_string:=2;
              output.Write(apostr,1);
            end;
            output.Write(c,1);
          end;
        end;
      end;
    end;

  end;
end;

procedure ObjectBinaryToCyr(input,output: TStream);
var temp: TMemoryStream;
begin
  temp:=TMemoryStream.Create;
  try
    ObjectBinaryToText(input,temp);
    temp.Seek(0,soFromBeginning);
    ObjectTextToCyr(temp,output);
  finally
    temp.Free;
  end;
end;

initialization
  RegisterStreamingFormat(sfCYR,'object',ObjectTextToBinary,ObjectBinaryToCyr);
end.
