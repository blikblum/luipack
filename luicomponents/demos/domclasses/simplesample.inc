  Tree := THtmlTree.Create;
  //Title := 'A Simple Html File';
  Tree.Add('p', 'Hello World');
  Tree.SaveToStream(Stream);
  Tree.Destroy;

