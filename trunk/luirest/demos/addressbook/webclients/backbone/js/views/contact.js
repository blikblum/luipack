app.ContactView = Backbone.View.extend({
 tagName: 'tr',
 template: Handlebars.compile(document.getElementById('contact-template').innerHTML),
 initialize: function(){
   this.listenTo(this.model, 'change', this.render)
 },
 render: function(){
   this.$el.html(this.template(this.model.toJSON()))
   return this;
 },
 //events
 events:{
   'click': 'click',
   'click a': 'linkClick'
 },
 click: function(e){
   this.model.trigger('contact:select', this.model)
 },
 linkClick: function(e){
    e.preventDefault();
    e.stopPropagation();
    if ($(e.currentTarget).hasClass('edit')){
      this.model.trigger('contact:edit', this.model)
    } else if ($(e.currentTarget).hasClass('delete')) {
      this.model.destroy();
    }
  }
}
)

app.ContactDialogView = Backbone.View.extend({
  className: 'modal-dialog',
  template: Handlebars.compile(document.getElementById('contact-dialog-template').innerHTML),
  initialize: function(options){
    this.$target = $(options.target);
  },
  render: function(){
    $('<div class="modal-content">')
      .appendTo(this.$el)
      .html(this.template(this.model.toJSON()));
    this.$target.empty().append(this.$el)
    return this
  },
  show: function(){
    this.$target.modal({backdrop: 'static'});
  },
  save: function(){
    var data = this.$('form').serializeObject();
    this.model.set(data);
    console.log('save model id ', this.model.get('id'), ' data: ', JSON.stringify(data) )
    if (this.model.isNew()){

      this.model.collection = this.collection;
      this.model.save();
      this.collection.add(this.model)
    } else {
      this.model.save();
    }
  },
  events:{
    'hidden.bs.modal': 'dialogHidden',
    'show.bs.modal': 'dialogShow',
    'click button.save': 'save'
  },
  dialogHidden: function(){
    this.remove()
    console.log('dialog hidden')
  },
  dialogShow: function(){
    console.log('dialog show')
  }
})
