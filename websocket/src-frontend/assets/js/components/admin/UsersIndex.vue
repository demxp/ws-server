<template>
  <div class="box">
    <div class="box-body">
      <div class="form-group top-block">
        <a class="btn btn-success" @click="$parent.$emit('switch-mode', {'mode': 'edituser', 'item': null})">Добавить</a>
      </div>
      <div class="table-responsive">
        <table class="table table-bordered table-striped">
          <thead>
            <tr>
              <th>Логин</th>
              <th>Имя</th>
              <th>Роль</th>
              <th>Действия</th>
            </tr>
          </thead>
          <tbody v-if="users.length == 0">
            <tr>
              <td colspan=5><center><h3>НЕТ ДАННЫХ</h3></center></td>
            </tr>
          </tbody>
          <tbody>
            <tr v-for="user in users" :class="{'tr__green':user.success, 'tr__red':user.danger}">
              <td>{{ user.login }}</td>
              <td>{{ user.name }}</td>
              <td>{{ user.role }}</td>
              <td>
                <button
                class="btn btn-xs btn-warning btn-block" 
                @click="edituser(user)"
                >Редактировать</button>                
                <button
                class="btn btn-xs btn-danger btn-block" 
                @click="deluser(user)"
                >Удалить</button>
              </td>   
            </tr>                 
          </tbody>
        </table>
      </div>
    </div>
  </div>
</template>

<script>
    import { MethodsMixin } from './../mixins/methods.mixin.js';

    export default {
      mixins: [MethodsMixin],
      command_list: 'list_users',
      command_delete: 'remove_user',
      mainArrayName: 'users',
      data(){
        return{
          users: []
        }
      },
      methods:{
        edituser(user){
          let fun = (user) => this.$parent.$emit('switch-mode', {'mode': 'edituser', 'item': user});
          if(user.main){
            this.requestPass(fun, user);
          }else{
            fun(user);
          }
        },
        deluser(user){
          let fun = (user) => {
            this.$root.ws.addCallback(this.$options.command_list, function(d){
              if('status' in d && d.status == 'ok'){
                this.fillTable(d);
              }
            }.bind(this), true);
            this.$root.wssend({
              action: this.$options.command_delete,
              id: user.id,
              old_password: user.old_password
            }, function(d){
              if('error' in d && d.error == 'notfound') return customAlert({text: "Пользователь не найден"});
              if('error' in d && d.error == 'badpassword') return customAlert({text: "Пароль не верен. Отмена."});
              this.$root.wssend({action: this.$options.command_list});
            }.bind(this), true);
          };          
          if(user.main){
            swal({
              title: "ВНИМАНИЕ!",
              text: "Удалить системную учетную запись невозможно, будут удалены лишь логин-сессии этой записи. Продолжить?",
              icon: "warning",
              buttons: true,
              dangerMode: true,
            })
            .then((willDelete) => {
              if (willDelete) {
                this.requestPass(fun, user);
              }
            });
          }else{
            this.deleteElem(user);
          }
        },
        requestPass(fun, input){
          swal({
            content: "input",
            title: "ВНИМАНИЕ!",
            text: "Вы пытаетесь изменить системную учетную запись. Введите текущий пароль этой записи:",
            icon: "warning",
          }).then((value) => {
            if(value.length < 1) return swal("Oops!", "Нужно ввести пароль...", "error");
            input.old_password = value;
            fun(input);
          })
        }
      }
    }
</script>

<style>
.user-avatar{
  text-align: center;
  max-width: 120px;
}

.user-avatar img {
    width: 100%;
    max-width: 70px;
    height: auto;
}  
</style>