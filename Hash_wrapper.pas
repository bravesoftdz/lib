unit Hash_wrapper;

//problem is, Delphi and Lazarus uses different libs for computing hash, so we should
//put all platform-dependent code here and define cross-platform THash

interface

type

THash = array [0..3] of LongWord; //from IdHash (Indy) declaration



implementation

end.
