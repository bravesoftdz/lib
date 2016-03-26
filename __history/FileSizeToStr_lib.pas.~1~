unit FileSizeToStr_lib;

interface

function FileSizeToStr(size: Int64): string;

var WriteKiBasKB: boolean;

implementation

uses SysUtils;

const megabyteSize = 1024*1024;
      gigabyteSize = 1024*1024*1024;
      terabyteSize: Int64 = Int64(1024*1024*1024)*1024;

resourcestring
  Bstr = 'ม';
  KiBstr = 'ส่ม';
  MiBstr = 'ฬ่ม';
  GiBstr = 'ร่ม';
  TiBstr = 'า่ม';
  KBstr = 'สม';
  MBstr = 'ฬม';
  GBstr = 'รม';
  TBstr = 'าม';


function FileSizeToStr(size: Int64): string;
begin
  if size<1024 then Result:=Format('%d %s',[size,Bstr])
  else if size<megabyteSize then
    if WriteKiBasKB then
      Result:=Format('%.3f %s',[size/1024,KBstr])
    else
      Result:=Format('%.3f %s',[size/1024,KiBstr])
  else if size<gigabyteSize then
    if WriteKiBasKB then
      Result:=Format('%.3f %s',[size/megabyteSize,MBstr])
    else
      Result:=Format('%.3f %s',[size/megabyteSize,MiBstr])
  else if size<terabyteSize then
    if WriteKiBasKB then
      Result:=Format('%.3f %s',[size/gigabyteSize,GBstr])
    else
      Result:=Format('%.3f %s',[size/gigabyteSize,GiBstr])
  else
    if WriteKiBasKB then
      Result:=Format('%.3f %s',[size/terabyteSize,TBstr])
    else
      Result:=Format('%.3f %s',[size/terabyteSize,TBstr])
end;

end.
