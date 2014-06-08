define(['handlebars'], function(Handlebars) {

this["JST"] = this["JST"] || {};

this["JST"]["app/scripts/templates/patient.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  var buffer = "", stack1, functionType="function", escapeExpression=this.escapeExpression;


  buffer += "<td>";
  if (stack1 = helpers.name) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.name; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "</td>\r\n<td></td>\r\n<td>\r\n  <a href=\"#/patients/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "\"><span class=\"glyphicon glyphicon-edit\"></span></a>\r\n  <a href=\"#/patients/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "/evaluations\"><span class=\"glyphicon glyphicon-list-alt\"></span></a>\r\n</td>\r\n\r\n\r\n";
  return buffer;
  });

this["JST"]["app/scripts/templates/patientedit.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  var buffer = "", stack1, self=this;

function program1(depth0,data) {
  
  
  return "Editar";
  }

function program3(depth0,data) {
  
  
  return "Adicionar";
  }

  buffer += "<h2>";
  stack1 = helpers['if'].call(depth0, depth0.id, {hash:{},inverse:self.program(3, program3, data),fn:self.program(1, program1, data),data:data});
  if(stack1 || stack1 === 0) { buffer += stack1; }
  buffer += " Paciente</h2>\r\n\r\n\r\n<form role=\"form\">\r\n  <fieldset>\r\n    <legend>Cadastro</legend>\r\n  <div class=\"row\">\r\n      <div class=\"form-group col-xs-4\">\r\n        <label for=\"patient-registry\">Registro</label>\r\n        <input id=\"patient-registry\" name=\"patient-registry\" type=\"text\" class=\"form-control\" disabled/>\r\n      </div>\r\n      <div class=\"form-group col-xs-8\">\r\n        <label for=\"patient-name\">Nome</label>\r\n        <input id=\"patient-name\" name=\"patient-name\" type=\"text\" class=\"form-control\"/>\r\n      </div>\r\n  </div>\r\n  <div class=\"row\">\r\n    <div class=\"form-group col-xs-4\">\r\n      <label for=\"patient-birthdate\">Data de Nascimento</label>\r\n      <input id=\"patient-birthdate\" name=\"patient-birthdate\" type=\"text\" class=\"form-control\"/>\r\n    </div>\r\n    <div class=\"form-group col-xs-4\">\r\n      <label for=\"patient-gender\">Gênero</label>\r\n      <input id=\"patient-gender\" name=\"patient-gender\" type=\"text\" class=\"form-control\"/>\r\n    </div>\r\n  </div>\r\n  </fieldset>\r\n  <fieldset>\r\n    <legend>Dados do Internamento</legend>\r\n    <div class=\"row\">\r\n      <div class=\"form-group col-xs-4\">\r\n        <label for=\"patient-originationid\">Origem</label>\r\n        <input id=\"patient-originationid\" name=\"patient-originationid\" type=\"text\" class=\"form-control\"/>\r\n      </div>\r\n      <div class=\"form-group col-xs-4\">\r\n        <label for=\"patient-internmentdate\">Data de Internamento</label>\r\n        <input id=\"patient-internmentdate\" name=\"patient-internmentdate\" type=\"text\" class=\"form-control\"/>\r\n      </div>\r\n      <div class=\"form-group col-xs-4\">\r\n        <label for=\"patient-internmenttypeid\">Tipo de Internamento</label>\r\n        <input id=\"patient-internmenttypeid\" name=\"patient-internmenttypeid\" type=\"text\" class=\"form-control\"/>\r\n      </div>\r\n    </div>\r\n  <div class=\"checkbox\">\r\n    <label>\r\n      <input type=\"checkbox\"/>Reinternamento</label>\r\n  </div>\r\n  <div class=\"checkbox\">\r\n    <label>\r\n      <input type=\"checkbox\"/>Reinternamento em 48h</label>\r\n  </div>\r\n  </fieldset>\r\n  <fieldset>\r\n    <legend>Dados Clínicos</legend>\r\n  <div class=\"row\">\r\n  <div class=\"form-group col-xs-4\">\r\n    <label for=\"patient-diagnosticid\">Diagnóstico</label>\r\n    <input id=\"patient-diagnosticid\" name=\"patient-diagnosticid\" type=\"text\" class=\"form-control\"/>\r\n  </div>\r\n  <div class=\"form-group col-xs-4\">\r\n    <label for=\"patient-apache2\">Apache2</label>\r\n    <input id=\"patient-apache2\" name=\"patient-apache2\" type=\"text\" class=\"form-control\"/>\r\n  </div>\r\n  <div class=\"form-group col-xs-4\">\r\n    <label for=\"patient-saps3\">SAPS3</label>\r\n    <input id=\"patient-saps3\" name=\"patient-saps3\" type=\"text\" class=\"form-control\"/>\r\n  </div>\r\n  </div>\r\n    <label>Comorbidades</label>\r\n    <div class=\"row\">\r\n    <div class=\"col-md-3\">\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>ICC</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>IRC</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>DCPF</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>DPOC</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Tumor Hematológico</label>\r\n    </div>\r\n    </div>\r\n    <div class=\"col-md-3\">\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Tumor Locoregional</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Metástase</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>HAS</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>DM</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>IAM Prévio</label>\r\n    </div>\r\n    </div>\r\n    <div class=\"col-md-3\">\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>AVC</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Deficit Visual</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Deficit Auditivo</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Demência</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Alcoolismo</label>\r\n    </div>\r\n    </div>\r\n    <div class=\"col-md-3\">\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Tabagismo</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Imunosupressão</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>SIDA</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Doença Reumática</label>\r\n    </div>\r\n    <div class=\"checkbox\">\r\n      <label>\r\n        <input type=\"checkbox\"/>Doença Psiquiátrica</label>\r\n    </div>\r\n    </div>\r\n    </div>\r\n  </fieldset>\r\n\r\n</form>\r\n<hr>\r\n<button type=\"button\" class=\"btn btn-default cancel\">Cancelar</button>\r\n<button type=\"button\" class=\"btn btn-primary save-model\">Salvar</button>\r\n\r\n";
  return buffer;
  });

this["JST"]["app/scripts/templates/patients.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  


  return "<div class=\"panel panel-default\">\r\n  <div class=\"panel-heading\">\r\n    <strong class=\"panel-title\">Pacientes</strong> <a href=\"#\" class=\"btn btn-primary btn-sm pull-right add-popover\" data-placement=\"bottom\" data-toggle=\"popover\" data-title=\"Insira o registro\" type=\"button\" data-html=\"true\"><span class=\"glyphicon glyphicon-plus-sign\"></span> Admitir Paciente</a>\r\n    <div class=\"hide add-popover-content\">\r\n      <form class=\"form-inline\" role=\"form\">\r\n        <div class=\"form-group\">\r\n          <input type=\"number\" class=\"form-control patient-registry\" placeholder=\"Registro\">\r\n        </div>\r\n        <button type=\"submit\" class=\"btn btn-primary add-patient\">Admitir</button>\r\n      </form>\r\n    </div>\r\n  </div>\r\n  <div class=\"panel-body\">\r\n    <table id=\"patients-table\" class=\"table table-hover\">\r\n      <thead>\r\n      </thead>\r\n      <tbody>\r\n      </tbody>\r\n    </table>\r\n  </div>\r\n</div>\r\n\r\n";
  });

return this["JST"];

});