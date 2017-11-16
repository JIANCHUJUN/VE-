import net.liftweb.json.DefaultFormats
import net.liftweb.json._

import scala.collection.mutable.ListBuffer
import scala.io.Source


case class RestaurantInfo (
                            name: String,
                            cuisine: String
                          )

case class DiningRecord (
                          name: String,
                          cuisine: String,
                          address: String,
                          timeOfVisit: String)
case class CuisineChoice (
						  cuisine: String,
						  var occurrence: Int,
						  var over_occurrence: Int,
						  var switch:Int)

object RestaurantRecommender {

  def main(args:Array[String]) {
    if (args.length != 1) {
      println("usage RestaurantRecommender diningHistoryFile.json")
      System.exit(-1)
    }
   

    /*
    * First, you have to anonymize the dining histories, by producing
    * the same sized List of List of String's, in which the String values
    * represent the cuisine entry from the original dining histories, ex:
    * [["Mexican", "Roman"], ["Japanese", "Japanese", "American"]]
    * */
    val (availableRestaurants, anonymizedHistories) = parseInput(args(0))
	
    makeRecommendation(availableRestaurants, anonymizedHistories)
  }

  /*
  * This function takes a list of available restaurants at the plaza
  * and a list of anonymized history lists and prints a recommendation
  * that should be formmated as
  * "Your group should consider going to: " + Restaurant Name
  */
  def makeRecommendation(availableRestaurants: List[RestaurantInfo], anonymizedHistories: List[List[String]]): Unit = {
		var cuisine_list = List[CuisineChoice]();
		cuisine_list = Recur1(cuisine_list,anonymizedHistories,0);
                    
                    println("cuisine_list.size = "+cuisine_list.size)
                    var i:Int = 0

                    for (i <- 0 to 7){
                        println(cuisine_list(i).cuisine + " " + cuisine_list(i).occurrence + " " + cuisine_list(i).over_occurrence)
                    }
		
		var unanimous_list = List[CuisineChoice]();
		var cui = CuisineChoice("Nothing",0,0,0);
		val (max_cui,max_unanimous) = Find_Recommendation1(cuisine_list, cui,cui, 0, anonymizedHistories.size, availableRestaurants);
		
		var find = find_cuisin_restaurant(max_unanimous.cuisine, availableRestaurants);
		if (find == -1)
		{
                        find = find_cuisin_restaurant(max_cui.cuisine, availableRestaurants);
                        if (find == -1)
                        {
                            println(availableRestaurants(0).name)
                            return;
                        }
		}
	        println(availableRestaurants(find).name)
		
		
		
  }
  
private def Find_Recommendation1(cuisine_list:List[CuisineChoice], max_unanimous:CuisineChoice, max_cui:CuisineChoice, index:Int, unanimous: Int, restaurant_list:List[RestaurantInfo]):(CuisineChoice, CuisineChoice) = {
		if (index < cuisine_list.size){
			var cui = cuisine_list(index);
                        var new_max_cui = max_cui;
                        var new_max_unanimous = max_unanimous;
			if (cui.occurrence > max_cui.occurrence){
				new_max_cui = cui;
			}
			if (cui.over_occurrence == unanimous && cui.occurrence > max_unanimous.occurrence){
                                var find = find_cuisin_restaurant(cui.cuisine, restaurant_list);
                                if (find != -1){
                                    new_max_unanimous = cui;
                                }
			}
			return Find_Recommendation1(cuisine_list, new_max_unanimous,new_max_cui, index+1, unanimous, restaurant_list);
		}
                return (max_cui, max_unanimous)
		
		
  }
  
  private def Recur1(cuisine_list:List[CuisineChoice], anonymizedHistories: List[List[String]], index:Int): List[CuisineChoice] = {
		if (index < anonymizedHistories.size)
		{
			var list:List[String] = anonymizedHistories(index);
			var new_cuisine_list = Recur2(cuisine_list,list,0)
			return Recur1(new_cuisine_list,anonymizedHistories,index+1);
		}
		return cuisine_list;
	
	}
  private def Recur2(cuisine_list:List[CuisineChoice], list: List[String], index:Int): List[CuisineChoice] = {
		if (index < list.size){
			var name = list(index);
			var find = find_cuisine(name,cuisine_list);
                        var new_cuisine_list = List[CuisineChoice]();
			if (find == -1){
				var cui = CuisineChoice(name,1,1,1);
				new_cuisine_list = cuisine_list :+ cui;
			}
			else{
				var cui = cuisine_list(find)
				cui.occurrence = cui.occurrence + 1;
				if (cui.switch != 1)
				{
					cui.over_occurrence = cui.over_occurrence + 1;
                                        cui.switch = 1;
				}
                                new_cuisine_list = cuisine_list;
			}
			return Recur2(new_cuisine_list, list, index+1);
		}
		return reswitch(cuisine_list, 0);	
		
  }
  private def reswitch(cuisine_list:List[CuisineChoice], index:Int):List[CuisineChoice] = {
  
		if (index == cuisine_list.size)
		{
			return cuisine_list;
		}
		var cui = cuisine_list(index);
		cui.switch = 0;
		return reswitch(cuisine_list, index+1);	
  }
  
  def find_cuisin_restaurant(cuisine_name:String, Restaurant_list:List[RestaurantInfo]):Int = {
		return return Find_Cuisine_Restaurant(cuisine_name,Restaurant_list,0);
  }
  private def Find_Cuisine_Restaurant(cuisine_name:String,  Restaurant_list:List[RestaurantInfo], index:Int):Int = {
	if (index == Restaurant_list.size)
	{
		return -1;
	}
	if (cuisine_name == Restaurant_list(index).cuisine)
	{
		return index;
	}
	return Find_Cuisine_Restaurant(cuisine_name,Restaurant_list,index+1);
  }
  
  def find_cuisine(cuisine_name:String, cuisine_list:List[CuisineChoice]):Int = {
		return Find_Cuisine(cuisine_name,cuisine_list,0);
  }
  private def Find_Cuisine(cuisine_name:String, cuisine_list:List[CuisineChoice], index:Int):Int = {
	if (index == cuisine_list.size)
	{
		return -1;
	}
	if (cuisine_name == cuisine_list(index).cuisine)
	{
		return index;
	}
	return Find_Cuisine(cuisine_name,cuisine_list,index+1);
  }
  

  def parseInput(fileName: String): (List[RestaurantInfo], List[List[String]]) = {
        
		val (restaurant_list, histories) = ParseInput(fileName)
		
		var availableRestaurants:List[RestaurantInfo] = restaurant_list;
		var anonymizedHistories = List[List[String]]();
		
		anonymizedHistories = Helper(anonymizedHistories, histories, 0);
		
		return (restaurant_list, anonymizedHistories)
	
	}
	
	private def Helper(anonymizedHistories: List[List[String]], Histories:List[List[DiningRecord]], index:Int):List[List[String]] = {
		if (index < Histories.size)
		{
			var record:List[DiningRecord] = Histories(index);
			var str = List[String](); 
			str = Helper2(str, record, 0);
			var new_anonymizedHistories = anonymizedHistories :+ str;
			return Helper(new_anonymizedHistories, Histories, index + 1)
		}
		return anonymizedHistories
	}
	
	private def Helper2(str: List[String], record:List[DiningRecord], index:Int):List[String] = {
		if (index < record.size){
			var din:DiningRecord = record(index)
			var new_str = str :+ din.cuisine;
			return Helper2(new_str, record, index + 1);
		}
		return str
	}
	
  /*
  * parseInput takes fileName, which represents a complete
  * path to a JSON file stored on disk.
  * This file contains a list of restaurants at a given shopping plaza and a list of
  * dining histories
  * If the input is successfully parsed, the function returns a tuple, in which
  * the first value is the List of available restaurants, while the second value
  * is the List of List of dining histories
  * */
 private def ParseInput(fileName: String): (List[RestaurantInfo], List[List[DiningRecord]]) = {
    implicit val formats = DefaultFormats
    val fileContents = Source.fromFile(fileName).getLines.mkString
    val json = parse(fileContents)
    val elements = (json \\ "Restaurant").children
    val restaurants = elements.map(ri => ri.extract[RestaurantInfo])

    val elements1 = (json \\ "dining history").children
    val histories = elements1.map(dh => dh.extract[JArray].children.map(h => h.extract[DiningRecord]))
    (restaurants, histories)
  }
}
