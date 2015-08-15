import Ember from 'ember';

export default Ember.Component.extend({
    actions: {
        changePage: function(current) {
            this.set('current', current);
            window.scrollTo(0,0);
        }
    },
    current: Ember.computed(function() {
        return 1;
    }),
    pageArr: Ember.computed('total','current', function() {
        var current = this.get('current');
        var total = this.get('total');
        var displayLength = 6;
        function Page(n) {
            return {
                number : n,
                clickable : parseInt(n),
                active : (current == n),
            }
        }
        var pageArr = [Page(current)];
        
        var left  = current - displayLength;
        var right = total - current - displayLength;
            
        function push(rlim) {
            for (var i = current + 1; i <= rlim; i++) {
                pageArr.push(Page(i));
            }
            pageArr.push(Page("..."));
            pageArr.push(Page(total));
        }
        
        function unshift(llim) {
            for (var i = current - 1; i >= llim; i--) {
                pageArr.unshift(Page(i));
            }
            pageArr.unshift(Page("..."));
            pageArr.unshift(Page(1));
        }
        
        function pushAll()  {
            for (var i = current + 1; i <= total; i++) {
                pageArr.push(Page(i));
            }
        }
        
        function unshiftAll() {
            for (var i = current - 1; i > 0; i--) {
                pageArr.unshift(Page(i));
            }
        }
        
        if (left + right <= 0) {
            unshiftAll();
            pushAll();
        } else if (left > 0 && right > 0) {
            push(current + displayLength - 2);
            unshift(current - displayLength + 2);
        } else {
            if (right > 0) {
                unshiftAll();
                push(current + displayLength - left - 2);
            } else if (left > 0) {
                pushAll();
                unshift(current - displayLength + right + 2);
            }
        }
        
        return pageArr;
    })
});
