/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'text!templates/predeliric.html',
  'stickitform',
  'validation'
], function ($, _, Backbone, html, StickitForm, Validation) {
  'use strict';

  var PredeliricView = Backbone.View.extend({
    html: html,

    bindings: function () {
      var bindings = StickitForm.getBindings({
        attributes: ['isurgency', 'morphine', 'hasinfection', 'coma', 'hassedation', 'urea', 'hasacidosis', 'apache2'],
        defaults: {
          setOptions: {
            validate: true
          }
        },
        extend: {
          morphine: {
            selectOptions: {
              collection: [
                {
                  'label': 'Sem morfina',
                  'value': 1
                },
                {
                  'label': '0,01 - 7,1mg',
                  'value': 2
                },
                {
                  'label': '7,2 - 18,6mg',
                  'value': 3
                },
                {
                  'label': '> 18,6mg',
                  'value': 4
                }
                       ],
              defaultOption: {
                'label': 'Selecione uma opção...',
                'value': null
              }
            }
          },
          coma: {
            selectOptions: {
              collection: [
                {
                  'label': 'Sem coma',
                  'value': 1
                },
                {
                  'label': 'Coma por medicação',
                  'value': 2
                },
                {
                  'label': 'Coma por outra causa',
                  'value': 3
                },
                {
                  'label': 'Coma por causa combinada',
                  'value': 4
                }
                            ],
              defaultOption: {
                'label': 'Selecione uma opção...',
                'value': null
              }
            }
          },
          isurgency: {
            onGet: 'boolToStr',
            onSet: 'strToBool'
          },
          hassedation: {
            onGet: 'boolToStr',
            onSet: 'strToBool'
          },
          hasinfection: {
            onGet: 'boolToStr',
            onSet: 'strToBool'
          },
          hasacidosis: {
            onGet: 'boolToStr',
            onSet: 'strToBool'
          },
          apache2: {
            onGet: 'numberToStr',
            onSet: 'strToNumber'
          },
          urea: {
            onGet: 'numberToStr',
            onSet: 'strToNumber'
          }
        }
      });

      _.extend(bindings, {
        '.risk-el': {
          observe: 'risk',
          onGet: function (val) {
            if (val != null && isFinite(val)) {
              return val.toFixed(1) + '%';
            } else {
              return '--'
            }
          }
        }
      });


      return bindings;
    },

    patientBindings: {
      '.name-el': 'name'
    },
    strToNumber: function (val) {
      if (val != null) {
        return +val;
      }
    },
    numberToStr: function (val) {
      if (val != null) {
        return val.toString();
      }
    },

    strToBool: function (val) {
      if (typeof val != 'undefined') {
        if (val === 'true') {
          return true
        } else {
          return false;
        }
      }
    },
    boolToStr: function (val) {
      if (typeof val != 'undefined') {

        if (val) {
          return 'true'
        } else {
          return 'false'
        }
      }
    },
    events: {
      'click button.save-model': 'saveModel',
      'click button.cancel': 'cancel'
    },

    initialize: function () {
      this.editModel = this.model.clone();
      Validation.bind(this, {
        model: this.editModel
      });
    },

    render: function () {
      this.$el.html(this.html);
      this.stickit(this.editModel);
      this.stickit(this.model.patient, this.patientBindings);
      return this;
    },
    remove: function () {
      Validation.unbind(this);
      return Backbone.View.prototype.remove.apply(this, arguments);
    },
    clearErrorMessage: function () {
      this.$('.alert-danger').addClass('hidden');
    },
    cancel: function () {
      //window.history.back();
      app.mainRouter.navigate('#patients/' + this.model.patient.get('id') + '/evaluations', true)
    },
    saveModel: function () {
      if (!this.editModel.isValid(true)) {
        this.$('.alert-danger').removeClass('hidden').html('Um ou mais campos contem dados inválidos');
        this.listenToOnce(this.editModel, 'validated', this.clearErrorMessage);
        return;
      }
      var self = this;
      this.model.save(this.editModel.attributes, {
        success: function (model, response, options) {
          console.log('Predeliric saved', model, response, options);
          app.mainRouter.navigate('#patients/' + model.get('patientid') + '/evaluations', true);
        },
        error: function (model, response, options) {
          var msg = '--';
          console.log('Error saving predeliric', model, response, options);
          if ((response.responseJSON) && (response.responseJSON.message)) {
            msg = response.responseJSON.message;
          }
          this.$('.alert-danger').removeClass('hidden').html('Erro ao salvar dados');
        }
      })
    }
  });

  return PredeliricView;
});