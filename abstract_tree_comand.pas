unit abstract_tree_comand;

interface

//����� ������� ��������� ����� ���� ������� ������ �� ���� ���������
  TAbstractTreeCommand=class(TAbstractCommand)
    private
      fNext,fPrev,fBranch: TAbstractTreeCommand;
      fTurnLeft: Boolean;
    protected
      fActiveBranch: Boolean;
    public
      procedure Clear; override;

      function EqualsByAnyOtherName(what: TStreamingClass): boolean; override;
    published
//�� ����� �� �� ��������� Prev � ����, � ��������������� ��� ��� ��������
//c�������� �������-������ ������, ����� 31 ���� �� �������
      property Prev: TAbstractTreeCommand read fPrev write fPrev;
      property Next: TAbstractTreeCommand read fNext write fNext;
      property Branch: TAbstractTreeCommand read fBranch write fBranch;
      property ActiveBranch: Boolean read fActiveBranch write fActiveBranch default false;
      property TurnLeft: Boolean read fTurnLeft write fTurnLeft default false;
    end;


implementation

 (*
        TAbstractTreeCommand
                                 *)
procedure TAbstractTreeCommand.Clear;
begin
  Next:=nil;
  Prev:=nil;
  Branch:=nil;
  ActiveBranch:=false;
  fImageIndex:=-1;
end;

function TAbstractTreeCommand.EqualsByAnyOtherName(what: TStreamingClass): boolean;
var t: TAbstractTreeCommand absolute what;
    buName: string;
    buNext,buPrev,buBranch: TAbstractTreeCommand;
    buActiveBranch,buTurnLeft: boolean;
    buHash: T4x4LongWordRecord;
    bin1,bin2: TMemoryStream;
    tmpName: string;
    ComponentWithSameName: TComponent;
begin
  if ClassType=what.ClassType then begin
    //����� ������� ����� ������� �����, ���������� �� ��������
    //��������� �� �� ��������� - �� ������, ��� ����������
    //�������� ����������� ���������...
    //������� ���������� ��� ������ ���������
    bin1:=TMemoryStream.Create;
    try
      bin1.WriteComponent(self);
      bin1.Seek(0,soFromBeginning);
//    self.saveFormat:=fCyr;
//    self.SaveToFile('wtf1.txt');
      //������ ����� �������� ��� ������ �������, ���� ��� �� ���������� ���������� � ����� ��
      //������
      tmpName:=Name;
      if Assigned(t.Owner) and (Name<>t.Name) and Assigned(t.Owner.FindComponent(Name)) then begin
        //��������� �������
        ComponentWithSameName:=t.Owner.FindComponent(Name);
        ComponentWithSameName.Name:='';
        //��������, ��� ��� ���� ��...
      end
      else ComponentWithSameName:=nil;
      //����� ������������� �������� ��������� "�� �����"
      buName:=t.Name;
      buNext:=t.next;
      buPrev:=t.prev;
      buBranch:=t.Branch;
      buActiveBranch:=t.ActiveBranch;
      buTurnLeft:=t.TurnLeft;
      t.Name:=tmpName;  //����� ��������, ��� �� ������������� ���� ����� �������� ���������
      t.Next:=Next;
      t.Prev:=Prev;
      t.Branch:=Branch;
      t.ActiveBranch:=ActiveBranch;
      t.TurnLeft:=TurnLeft;
      if t is THashedCommand then begin
        buHash:=HashedT.fHash;
        HashedT.fHash:=(self as THashedCommand).fHash;
      end;

      bin2:=TMemoryStream.Create;
      try
        bin2.WriteComponent(t);
        bin2.Seek(0,soFromBeginning);
        if bin1.Size<>bin2.Size then Result:=false
        else Result:=Comparemem(bin1.Memory,bin2.Memory,bin1.Size);
      finally
        bin2.free;
      end;
  //    if Result=false then begin
  //      what.saveFormat:=fCyr;
  //      what.SaveToFile('wtf2.txt');
  //    end;
    finally
      bin1.Free;
    end;

    t.Name:=buName;
    t.Next:=buNext;
    t.Prev:=buPrev;
    t.Branch:=buBranch;
    t.ActiveBranch:=buActiveBranch;
    t.TurnLeft:=buTurnLeft;

    if Assigned(ComponentWithSameName) then
      ComponentWithSameName.Name:=tmpName;
    if t is THashedCommand then
      HashedT.fHash:=buHash;
  end
  else Result:=false;
end;


end.
