from gateway.worldometer_gateway import WorldOMeterGateway
from services.parser_service import ParserService
from datetime import date, timedelta

if __name__ == "__main__":

    worldometer_gateway = WorldOMeterGateway()
    parser_service = ParserService()

    data = worldometer_gateway.fetch()
    output = parser_service.create_df_worldometer(data)
    output["today"].to_csv(r'./current.csv', index = False)
    yesterday = date.today() - timedelta(days=1)
    output["yesterday"].to_csv(yesterday.strftime("%Y-%m-%d")+".csv", index = False)

