unit detect_memory_leak;

interface

implementation

uses Windows;

initialization


finalization
  if AllocMemSize <> 0 then
    MessageBox(0, 'An unexpected memory leak has occurred.', 'Unexpected Memory Leak', MB_OK or MB_ICONERROR or MB_TASKMODAL);


end.
