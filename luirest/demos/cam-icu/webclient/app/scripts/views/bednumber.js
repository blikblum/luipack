/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'text!templates/bednumber.html'
], function ($, _, Backbone, html) {
  'use strict';

  var BednumberView = Backbone.View.extend({
    html: html,

    bindings: {
      '[name="bednumber"]': {
        observe: 'bednumber',
        onSet: function (val) {
          return +val;
        }
      },
      '.name-el': 'name'
    },

    events: {
      'click button.save-model': 'saveModel',
      'click button.cancel': 'cancel'
    },

    initialize: function () {
      this.editModel = this.model.clone();
    },

    render: function () {
      this.$el.html(this.html);
      this.stickit(this.editModel);
      return this;
    },
    cancel: function () {
      //window.history.back();
      app.mainRouter.navigate('#patients/' + this.model.get('id') + '/actions', true)
    },
    saveModel: function () {
      var attrs;
      var self = this;
      //todo: use Validation plugin
      if (!isFinite(this.editModel.get('bednumber'))) {
        alert('Leito deve ser um valor numérico');
        return;
      }
      attrs = _.pick(this.editModel.attributes, 'id', 'bednumber');
      this.model.save(attrs, {
        wait: true,
        patch: true,
        success: function (model, response, options) {
          console.log('Bednumber saved', model, response, options);
          app.mainRouter.navigate('#patients/' + model.get('id') + '/actions', true);
        },
        error: function (model, response, options) {
          var msg = '--';
          console.log('Error saving bednumber', model, response, options);
          if ((response.responseJSON) && (response.responseJSON.message)) {
            msg = response.responseJSON.message;
          }
        }
      })
    }
  });

  return BednumberView;
});