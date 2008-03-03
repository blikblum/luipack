  Tree := THtmlTree.Create;
  Tree.Title := 'A Table Built With Speciaized Functions';
  Table := TTableTree.Create(Tree);
  with Table do
  begin
    AddRow;

    AddCol('Row1Col1');
    AddCol('Row1Col2');
    AddCol('Row1Col3');
    AddRow;
    AddCol('Row2Col1');
    AddCol('Row2Col2');
    AddCol('Row2Col3');

    Attach;
    Destroy;
  end;
  Tree.SaveToStream(Stream);
  Tree.Destroy;

