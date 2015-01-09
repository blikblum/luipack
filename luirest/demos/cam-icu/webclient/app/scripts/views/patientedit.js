/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'text!templates/patientedit.html',
  'stickitform',
  'validation'
], function ($, _, Backbone, html, StickitForm, Validation) {
  'use strict';

  var PatienteditView = Backbone.View.extend({
    html: html,
    initialize: function () {
      this.editModel = this.model.clone();
      Validation.bind(this, {
        model: this.editModel
      });
    },
    bindings: function () {
      var bindings = StickitForm.getBindings({
        attributes: ['registry', 'name', 'bednumber', 'gender', 'birthdate', 'originationid', 'internmenttypeid',
            'diagnosticid', 'internmentdate', 'isreinternment', 'isreinternment48h', 'apache2', 'saps3',
            'hasicc', 'hasirc', 'hasdcpf', 'hasdpoc', 'hashematologytumor', 'haslocoregionaltumor', 'hasmetastasis',
            'hashas', 'hasdm', 'haspreviousiam', 'hassmoking', 'hasavc', 'hasvisualdeficit', 'hasauditorydeficit',
            'hasdementia', 'hasalcoholism', 'hasimmunosuppression', 'hassida', 'hasrheumaticdisorder', 'haspsychiatricdisorder'],
        defaults: {
          setOptions: {
            validate: true
          }
        },
        extend: {
          birthdate: {
            onGet: 'formatDate',
            onSet: 'parseDate'
          },
          internmentdate: {
            onGet: 'formatDate',
            onSet: 'parseDate'
          },
          gender: {
            selectOptions: {
              collection: [{
                'label': 'Masculino',
                value: 'M'
              }, {
                label: 'Feminino',
                value: 'F'
              }]
            }
          },
          bednumber: {
            onSet: function (val) {
              return +val;
            }
          },
          originationid: {
            selectOptions: {
              collection: [
                {
                  label: 'NC',
                  value: 9
                },
                {
                  label: 'Enfermaria',
                  value: 1
                },
                {
                  label: 'Centro Cirúrgico',
                  value: 2
                },
                {
                  label: 'Semi Intensiva',
                  value: 3
                },
                {
                  label: 'Emergência',
                  value: 4
                },
                {
                  label: 'Home Care',
                  value: 5
                },
                {
                  label: 'Outro Hospital',
                  value: 6
                },
                      ],
              defaultOption: {
                label: 'Selecione uma opção...',
                value: null
              }
            }
          },
          internmenttypeid: {
            selectOptions: {
              collection: [
                {
                  label: 'Clínico',
                  value: 2
                },
                {
                  label: 'Cirúrgico',
                  value: 1
                },
                {
                  label: 'Trauma',
                  value: 3
                },
                {
                  label: 'Neurologia / Neurocirurgia',
                  value: 4
                }
                      ],
              defaultOption: {
                label: 'Selecione uma opção...',
                value: null
              }
            }
          },
          diagnosticid: {
            selectOptions: {
              collection: [
                {
                  label: 'Sepse',
                  value: 1
                },
                {
                  label: 'Ins. Respiratória',
                  value: 10
                },
                {
                  label: 'Hematológico',
                  value: 11
                },
                {
                  label: 'Choque',
                  value: 12
                },
                {
                  label: 'Cirurgia torácica',
                  value: 13
                },
                {
                  label: 'Neurocirurgia',
                  value: 14
                },
                {
                  label: 'Cirurgia cardíaca',
                  value: 15
                },
                {
                  label: 'Cirurgia abdominal',
                  value: 16
                },
                {
                  label: 'Cirurgia ortopédica',
                  value: 17
                },
                {
                  label: 'Cirurgia de coluna',
                  value: 18
                },
                {
                  label: 'Cirurgia urológica',
                  value: 19
                },
                {
                  label: 'Renal',
                  value: 2
                },
                {
                  label: 'Cirurgia vascular',
                  value: 20
                },
                {
                  label: 'Neurológico',
                  value: 3
                },
                {
                  label: 'Trauma',
                  value: 4
                },
                {
                  label: 'Hepático',
                  value: 5
                },
                {
                  label: 'Cardiovascular',
                  value: 6
                },
                {
                  label: 'Digestivo',
                  value: 7
                },
                {
                  label: 'Monitorização',
                  value: 8
                },
                {
                  label: 'Pós-PCR',
                  value: 9
                }
                      ],
              defaultOption: {
                label: 'Selecione uma opção...',
                value: null
              }
            }
          }
        }
      });

      return bindings;
    },
    formatDate: function (val) {
      if (val) {
        return fromOADate(val).toLocaleDateString('pt-BR');
      }
    },
    parseDate: function (val) {
      var parts;
      var date;

      if (val) {
        parts = val.split('/');
        date = new Date(parts[2], parts[1] - 1, parts[0]);
        return toOADate(date);
      }
    },
    render: function () {
      var title = (this.model.isNew()) ? 'Adicionar' : 'Editar';
      title += ' Paciente';
      this.$el.html(this.html);
      this.$('.title-el').html(title);
      this.$('.date-control').mask('99/99/9999');
      this.stickit(this.editModel);
      return this;
    },
    remove: function () {
      Validation.unbind(this);
      return Backbone.View.prototype.remove.apply(this, arguments);
    },
    events: {
      'click button.save-model': 'saveModel',
      'click button.cancel': 'cancel',
      'click button.settoday-el': 'setInternmentDateToday'
    },
    cancel: function () {
      if (this.model.isNew()) {
        app.mainRouter.navigate('#patients', {
          trigger: true
        });
      } else {
        app.mainRouter.navigate('#patients/' + this.model.get('id') + '/actions', {
          trigger: true
        });
      }
    },
    saveModel: function () {
      var self = this;
      //do not validate discharge fields
      var validationAttrs = _.keys(_.omit(this.model.validation, 'dischargedate', 'dischargereasonid'))
        //todo: add workaround to Validation plugin bug https://github.com/thedersen/backbone.validation/issues/205
      if (!this.editModel.isValid(validationAttrs)) {
        this.$('.alert-danger').removeClass('hidden').html('Um ou mais campos contem dados inválidos');
        return;
      }

      if (this.model.isNew()) {
        this.model.collection = this.collection;
        this.model.save(this.editModel.attributes, {
          wait: true,
          success: function (model) {
            console.log('Paciente salvo', model);
            self.collection.add(self.model);
            app.mainRouter.navigate('#patients', {
              trigger: true
            });
          },
          error: function (model, response, options) {

            console.log('Erro ao salvar os dados: ', response, options);
            this.$('.alert-danger').removeClass('hidden').html('Erro ao salvar dados');
          }
        });

      } else {
        this.model.save(this.editModel.attributes, {
          success: function (model) {
            console.log('Paciente salvo', model);
            app.mainRouter.navigate('#patients', {
              trigger: true
            });
          },
          error: function (model, response, options) {
            console.log('Erro ao salvar os dados: ', response, options);
            this.$('.alert-danger').removeClass('hidden').html('Erro ao salvar dados');
          }
        });
      }
    },
    setInternmentDateToday: function (e) {
      e.preventDefault()
      this.editModel.set('internmentdate', toOADate(new Date()));
    }
  });

  return PatienteditView;
});