from bs4 import BeautifulSoup
import pandas as pd

class ParserService:

    @staticmethod
    def create_df_worldometer(raw_data):
        """
        Parses the raw HTML response from Worldometer and returns a DataFrame from it

        @Params:
        raw_data (string): request.text from Worldometer

        @Returns:
        DataFrame
        """
        soup = BeautifulSoup(raw_data, features="html.parser")

        result = dict()
        for day in ["today", "yesterday"]:
            _id = "main_table_countries_"+day

            table = soup.find("table", attrs={"id": _id});

            columns = [col.get_text() for col in table.find("thead").findAll("th")]

            countries_data = table.find("tbody").findAll("tr")

            parsed_data = []
            for country in countries_data:
                parsed_data.append([data.get_text() for data in country.findAll("td")])

            result[day] = pd.DataFrame(parsed_data, columns=columns)

        return result