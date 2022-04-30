<template>
    <!-- Site wrapper -->
  <div class="wrapper">
    <header class="main-header">
      <!-- Logo -->
      <a href="/admin" class="logo">
        <!-- mini logo for sidebar mini 50x50 pixels -->
        <span class="logo-mini"><b>A</b>LT</span>
        <!-- logo for regular state and mobile devices -->
        <span class="logo-lg"><b>Admin</b>LTE</span>
      </a>
      <!-- Header Navbar: style can be found in header.less -->
      <nav class="navbar navbar-static-top">
        <!-- Sidebar toggle button-->
        <a href="#" class="sidebar-toggle" data-toggle="offcanvas" role="button">
          <span class="sr-only">Toggle navigation</span>
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
        </a>
      </nav>
    </header>
    <!-- Left side column. contains the sidebar -->
    <aside class="main-sidebar">
      <!-- sidebar: style can be found in sidebar.less -->
      <section class="sidebar">
        <!-- sidebar menu: : style can be found in sidebar.less -->
        <ul class="sidebar-menu">
          <li class="header">ГЛАВНОЕ МЕНЮ</li>
          <li>
            <a href="#" @click="$emit('switch-mode', {'mode': 'users', 'id': null})">
              <i class="fa fa-users"></i> <span>Пользователи</span>
            </a>
          </li>
        </ul>
      </section>
      <!-- /.sidebar -->
    </aside>
    <!-- =============================================== -->

    <!-- Content Wrapper. Contains page content -->
    <div class="content-wrapper">
      <!-- Content Header (Page header) -->
      <section class="content-header">
        <h1>Привет! Это админка. Выберите раздел слева...</h1>
      </section>

      <!-- Main content -->
      <section class="content">
          <users-index v-if="checkMode('users')"></users-index>
          <users-edit v-if="checkMode('edituser')" :user-set="item"></users-edit>
      </section>
      <!-- /.content -->
    </div>
    <!-- /.content-wrapper -->

    <footer class="main-footer">
      <div class="pull-right hidden-xs">
        <b>Version</b> 2.3.7
      </div>
      <strong>Copyright &copy; 2014-2016 <a href="http://almsaeedstudio.com/">Almsaeed Studio</a>.</strong> All rights
      reserved.
    </footer>
  </div>
  <!-- ./wrapper -->
</template>

<script>
  export default {
    data(){
      return{
        modes:[
          'users',
          'edituser'
        ],
        current: 'index',
        item: null
      }
    },
    mounted(){
      this.$on('switch-mode', this.setMode);
    },
    methods:{
      setMode(event){
        if(!!this.modes.find((el) => el === event.mode)){
          this.current = event.mode;
          if(event.item !== null){
            this.item = event.item;
          }else{
            this.item = null;
          }
        }else{
          this.current = 'index';
          this.item = null;        
        }
      },
      checkMode(mode){
        return this.current == mode;
      }
    }       
  }
</script>