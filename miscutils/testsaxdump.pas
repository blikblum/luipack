program saxdump;

{$mode objfpc}
//Todo: 
// -add commandline
// -see redirection
// -add depth handling

uses sysutils,classes,sax,sax_html;

type
  TSaxDumper = class (THtmlReader)
  private
    procedure EndElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
    procedure StartElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
    procedure Characters(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
  public
    constructor Create;
  end;
  
var 
  Dumper: TSaxDumper;

constructor TSaxDumper.Create;
begin
  inherited Create;;
  OnStartElement:= @StartElement;
	OnEndElement:= @EndElement;
	OnCharacters:= @Characters;
end;  

procedure TSaxDumper.EndElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  writeln('= End Element =');
  writeln('NamespaceURI: ',NamespaceURI);
  writeln('LocalName: ',LocalName);
  writeln('QName: ',QName);
  writeln(' ');
end;  

procedure TSaxDumper.StartElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);	
var
  Counter:Integer;
begin
  writeln('= Start Element =');
  writeln('NamespaceURI: ',NamespaceURI);
  writeln('LocalName: ',LocalName);
  writeln('QName: ',QName);
  if Atts <> nil then
  begin
  writeln('*Attributes*');  
  with Atts do
    for Counter := 0 to Atts.length - 1 do
    begin
      writeln('Attribute No ',Counter);
      writeln('  LocalNames[',Counter,']: ',LocalNames[Counter]);
      writeln('  QNames[',Counter,']: ',QNames[Counter]);
      writeln('  Types[',Counter,']: ',Types[Counter]);
      //writeln('URIS[',Counter,']: ',URIS[Counter]);
      writeln('  Values[',Counter,']: ',Values[Counter]);
    end;   
  end;  
  writeln(' ');
end;  	

procedure TSaxDumper.Characters(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
begin
  writeln('= Characters =');
  writeln('ch: ',WideString(ch));
  writeln('AStart: ',IntToStr(AStart));
  writeln('ALength: ',IntToStr(ALength));
  writeln(' ');
end;  

begin
	//assign(output,'sax.log');
	//rewrite(output);
	if ParamCount <> 1 then
	begin
	  writeln('Wrong number of parameters');
	  writeln('Usage: saxdump filename');
	  halt;
	end else 
  	if not FileExists(ParamStr(1)) then
  	begin
  	  writeln('File ',ParamStr(1),' doesnt exists');
  	  halt;
  	end; 
	     
	Dumper:= TSaxDumper.Create;
	with Dumper do
	begin
	  Parse (ParamStr(1));
	  Destroy;
	end; 
  //assign(output,'');	
end.