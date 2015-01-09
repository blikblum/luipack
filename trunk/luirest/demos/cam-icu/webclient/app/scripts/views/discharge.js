/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'text!templates/discharge.html',
  'stickitform',
  'validation'
], function ($, _, Backbone, html, StickitForm, Validation) {
  'use strict';

  var DischargeView = Backbone.View.extend({
    html: html,

    bindings: function () {
      var bindings = StickitForm.getBindings({
        attributes: ['dischargedate', 'dischargereasonid'],
        extend: {
          dischargedate: {
            onGet: 'formatDate',
            onSet: 'parseDate'
          },
          dischargereasonid: {
            selectOptions: {
              collection: [{
                  label: 'Melhora',
                  value: 1
                },
                {
                  label: 'Óbito',
                  value: 2
                },
                {
                  label: 'NC',
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
      })
      _.extend(bindings, {
        '.name-el': 'name'
      })
      return bindings;
    },

    events: {
      'click button.save-model': 'saveModel',
      'click button.cancel': 'cancel',
      'click button.settoday-action': 'setDischargeDateToday'
    },

    initialize: function () {
      this.editModel = this.model.clone();
      Validation.bind(this, {
        model: this.editModel
      });
    },
    remove: function () {
      Validation.unbind(this);
      return Backbone.View.prototype.remove.apply(this, arguments);
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
      this.$el.html(this.html);
      this.$('.date-control').mask('99/99/9999');
      this.stickit(this.editModel);
      return this;
    },
    clearErrorMessage: function () {
      this.$('.alert-danger').addClass('hidden');
    },
    cancel: function () {
      //window.history.back();
      app.mainRouter.navigate('#patients/' + this.model.get('id') + '/actions', true)
    },
    saveModel: function () {
      var attrs;
      var self = this;
      //todo: add workaround to Validation plugin bug https://github.com/thedersen/backbone.validation/issues/205
      if (!this.editModel.isValid(['dischargedate', 'dischargereasonid'])) {
        this.$('.alert-danger').removeClass('hidden').html('Um ou mais campos contem dados inválidos');
        this.listenToOnce(this.editModel, 'validated', this.clearErrorMessage);
        return;
      }
      attrs = _.pick(this.editModel.attributes, 'id', 'dischargedate', 'dischargereasonid');
      this.model.save(attrs, {
        wait: true,
        patch: true,
        success: function (model, response, options) {
          console.log('Discharge saved', model, response, options);
          app.data.patients.remove(self.model);
          app.mainRouter.navigate('#patients', true);
        },
        error: function (model, response, options) {
          var msg = '--';
          console.log('Error saving discharge', model, response, options);
          if ((response.responseJSON) && (response.responseJSON.message)) {
            msg = response.responseJSON.message;
          }
          this.$('.alert-danger').removeClass('hidden').html('Erro ao salvar dados');
        }
      })
    },
    setDischargeDateToday: function (e) {
      e.preventDefault();
      this.editModel.set('dischargedate', toOADate(new Date()));
    }
  });

  return DischargeView;
});