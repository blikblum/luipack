  Tree := THtmlTree.Create;
  with Tree do
  begin
    Title := 'List Created With Raw Functions';
    SubTree.AddBase('ul');
    SubTree.Add('li');
    SubTree.AddText('List Created with');
    SubTree.Add('li');
    SubTree.AddText('Raw Functions');
    SubTree.Attach;
    SaveToStream(Stream);
    Destroy;
  end;

