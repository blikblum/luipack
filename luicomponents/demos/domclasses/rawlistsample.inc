  Tree := THtmlTree.Create;
  //Tree.Title := 'List Created With Raw Functions';
  Tree
  .Open('ul')
     .Add('li', 'List Created with')
     .Add('li', 'Raw Functions')
  .Close;
  Tree.SaveToStream(Stream);
  Tree.Destroy;

