app.PhoneView = Backbone.View.extend({
  tagName: 'tr',
  template: Handlebars.compile(document.getElementById('phone-template').innerHTML),
  initialize: function(){
    this.listenTo(this.model, 'destroy', this.remove)
  },
  render: function(){
    this.$el.append(this.template(this.model.toJSON()))
    return this
  },
  //events
  events:{
    'click a': 'linkClick'
  },
  linkClick: function(e){
    e.preventDefault;
    if ($(e.currentTarget).hasClass('edit')){

    } else if ($(e.currentTarget).hasClass('delete')) {
      this.model.destroy();
    }
  }
})