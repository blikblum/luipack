  Tree := THtmlTree.Create;
  with Tree do
  begin
    Title := 'A Simple Html File';
    SubTree.Add('p');
    SubTree.AddText('Hello World');
    SubTree.Attach;
    SaveToStream(Stream);
    Destroy;
  end;

