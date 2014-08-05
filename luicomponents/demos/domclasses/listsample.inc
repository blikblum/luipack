  Tree := THtmlTree.Create;
  with Tree, SubTree do
  begin
    Title := 'List Created With Specialized Functions';
    AddBase('ul');
    AddListItem('First Item of a');
    AddListItem('List Created');
    AddListItem('With Especialized list functions');
    Attach;
    SaveToStream(Stream);
    Destroy;
  end;

