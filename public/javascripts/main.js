var PokerApp = angular.module('PokerApp', ['ngWebsocket']);

PokerApp.controller("MainController", function ($scope, $websocket) {
    $scope.stuff = " amazing angular!";
    $scope.players = [];
    $scope.messages = [];
    $scope.joined = false;
    $scope.pot = 0;

    $scope.table = [];
    $scope.hand = [];

    var ws = $websocket.$new('ws://localhost:9000/websocket');

    $scope.join = function () {
        console.log("joining with name = " + $scope.playerName);

        ws.$emit("join", {position: $scope.playerPosition, name: $scope.playerName});

        $scope.joined = true;
        $scope.$apply();
    };

    $scope.start = function () {
        console.log("starting game... ");

        ws.$emit("startGame")
    };

    $scope.call = function () {
        ws.$emit("call")
    };

    $scope.check = function () {
        ws.$emit("check")
    };

    $scope.fold = function () {
        ws.$emit("fold")
    };

    $scope.bet = function () {
        console.log("betting this amount: " + $scope.betAmount);
        ws.$emit("bet", {"bet": $scope.betAmount});
    };

    ws.$on('$open', function () {
        console.log('WebSocket connection open!');
    });

    ws.$on('newplayer', function (data) {
        $scope.players = data;
        $scope.$apply();
    });

    ws.$on('hand', function (data) {
        $scope.hand = data;
        $scope.$apply();
    });

    ws.$on('tablepot', function (data) {
        $scope.pot = data;
        $scope.$apply();
    });

    ws.$on('tablecards', function (data) {
        $scope.table = data;
        $scope.$apply();
    });

    ws.$on('chatMessage', function (data) {
        $scope.messages.unshift(data);
        $scope.$apply();
    });

    ws.$on('gameMessage', function (data) {
        $scope.messages.unshift(data);
        $scope.$apply();
    });

    ws.$on('$message', function (data) {
        console.log("Some message was received: ");
        console.log(data)
    });
});

PokerApp
    .filter('trusted', ['$sce', function($sce){
        return function(text) {
            return $sce.trustAsHtml(text);
        };
    }])
    .filter('cardClass', function () {
        return function (text) {
            var color = ['black', 'red', 'red', 'black'][Math.floor(text / 13)];
            return 'card card-' + color;
        };
    })
    .filter('cardFace', ['$sce', function($sce){
        return function(text) {
            var ranks = {
                11: 'J', 12: 'Q', 13: 'K', 14: 'A'
            };
            var rank = 2+(text%13);
            if (rank > 10) rank = ranks[rank];
            var suit = ['spade', 'heart', 'diamond', 'club'][Math.floor(text/13)];
            return $sce.trustAsHtml(rank + "<span class='suit'>&"+ suit + "suit;</span>");
        };
    }]);



PokerApp.directive('card', function () {
    return {
        priority: 1001,
        replace: true,
        template: "<div class='card card-{{ color }}'>{{ rank }}<span class='suit' ng-bind-html='suit | trusted'></span></div>",
        link: function ($scope, $element, attrs, $sce) {
            var id = 100;
            console.log(attrs.ngCard);
            $scope.color = ['black', 'red', 'red', 'black'][Math.floor(id/14)];
            $scope.rank = 2;
            $scope.suit = "&clubsuit;";
        }

        /*function (elem, attr) {
            console.log("directive is active");
            console.log(attr.no);
            var ranks = {
                11: 'J', 12: 'Q', 13: 'K', 14: 'A'
            };
            var suits = ['spade', 'heart', 'diamond', 'club'];
            var colors = ['black', 'red', 'red', 'black'];
            var id = card;
            var suit = suits[Math.floor(id/14)];
            var color = colors[Math.floor(id/14)];
            var rank = 1+id%14;
            if (rank > 10) this.rank = ranks[this.rank];

            return "<div class='card card-"+color+"'>"+rank+"<span class='suit'>&"+suit+"suit;</span></div>"
        }*/
    }
});