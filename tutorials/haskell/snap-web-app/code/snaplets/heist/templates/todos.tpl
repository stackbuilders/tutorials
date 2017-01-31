<apply template="base">
  <div class="col-xs-6 col-xs-offset-3">
    <div class="well">
      <h1>TODO Items</h1>
      <ul class="list-group">
        <todos>
          <input type="hidden" value="${id}">
          <li class="list-group-item clearfix ${successClass}">
            <span><description/></span>
            <div class="pull-right" role="group">
              <a class="btn btn-xs btn-success img-circle"
                  onclick="document.getElementById('form-check-${id}').submit();">
                  ✓
              </a>
              <form id="form-check-${id}" action="/todos/${id}/complete/${notCompleted}" method="post" class="hidden-form">
              </form>
              <span> </span>
              <a class="btn btn-xs btn-danger img-circle"
                  onclick="document.getElementById('form-rmv-${id}').submit();">
                Ｘ
              </a>
              <form id="form-rmv-${id}" action="/todos/${id}/delete" method="post" class="hidden-form">
              </form>
            </div>
          </li>
        </todos>
      </ul>
      <div>
        <hr>
        <div class="clearfix">
          <form class="form-horizontal" action="/todos/new" method="post">
            <div class="form-group">
              <label for="description" class="col-md-2 control-label">Task</label>
              <div class="col-md-10">
                <input type="text" name="description" class="form-control" placeholder="What do you need to do?" >
              </div>
            </div>
            <div class="row">
              <div class="col-md-10 col-md-offset-2 text-right">
                <input type="submit" value="Save Item" class="btn btn-primary">
              </div>
            </div>
          </form>
        </div>
      </div>
    </div>
  </div>
</apply>
