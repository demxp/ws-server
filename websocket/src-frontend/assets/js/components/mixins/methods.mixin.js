export const MethodsMixin = {
  mounted(){
    if(!!this.$options.command_list){
      this.$root.wssend({action: this.$options.command_list}, this.fillTable, true);
    }
  },  
  methods: {
    fillTable(data){
      data = data.data;
      let an = this.$options.mainArrayName;
      data.map((item, i) => {
        item.success = false;
        item.danger = false;
        if(!!this[an][i]){
          Object.keys(item).map((param) => this[an][i][param] = item[param]);
        }else{
          this[an].push(item)
        }
      });
      if(data.length < this[an].length){
        this[an].splice(data.length, this[an].length - data.length);
      }
    },
    deleteElem(data){
      if(!confirm("Вы уверены?")){return false;}
      this.$root.ws.addCallback(this.$options.command_list, function(d){
        if('status' in d && d.status == 'ok'){
          this.fillTable(d);
        }
      }.bind(this), true);
      this.$root.wssend({
        action: this.$options.command_delete,
        id: data.id
      }, function(d){
        if('error' in d && d.error == 'notfound') return customAlert({text: "Пользователь не найден"});
        if('error' in d && d.error == 'badpassword') return customAlert({text: "Пароль не верен. Отмена."});
        this.$root.wssend({action: this.$options.command_list});
      }.bind(this), true);
    } 
  }
}